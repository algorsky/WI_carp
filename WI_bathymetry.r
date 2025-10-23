wingra = st_read('data/map/yld_study_lakes.shp')|>
  filter(LAKEID == "WI")
ggplot(wingra) + geom_sf() + 
  geom_sf(data = boundary, alpha = 0.5, fill = 'blue')

points = read_csv('data/map/DNR_pointsurvey/Wingra_PI_points.csv')
points

ggplot(points) +
  geom_jitter(aes(x = Longitude, y = Latitude, col = as.character(Depth_ft)), width = 0.001, height = 0.001)
table(points$Depth_ft)


library(tidyverse)
library(sf)
library(ggspatial)
library(patchwork)
library(smoothr)
library(scales)

# Load lake outline
boundary = st_read('data/map/WingraBathymetry2025/WingraBathy_0.5.shp') %>% 
  dplyr::filter(Depth == 0)
ggplot(boundary) + geom_sf()

# Load bathymetry data
dnr_points = read_csv('data/map/DNR_pointsurvey/Wingra_PI_points.csv')

# # Convert csv to shapefile
sb.sf = st_as_sf(dnr_points, coords = c("Longitude", "Latitude"),
                 crs = 4326, agr = "constant") %>%
  mutate(Depth_m = Depth_ft * 0.3048) %>% 
  filter(!is.na(Depth_m)) %>% 
  select(Depth_m) %>% 
  st_transform(crs = st_crs(boundary))
# st_write(obj = sb.sf, dsn = 'SB_bathy.shp', delete_dsn = T)

ggplot(wingra) + geom_sf() + 
  geom_sf(data = boundary, alpha = 0.5, fill = 'blue') +
  geom_sf(data = sb.sf, alpha = 0.5, fill = 'red')


# sb.sf = st_read('SB_rawdepths.shp') %>% 
#   mutate(Depth = Depth + 0.165) # Sensor depth

# Plot raw data
ggplot() + geom_sf(data = sb.sf, aes(color = Depth_m)) +
  geom_sf(data = boundary, fill = NA, color = 'red3') +
  theme_bw()

# Convert to a data frame with X, Y, and depth values
boundary_points <- st_cast(boundary, "POINT") %>% mutate(Depth_m = 0) %>% 
  select(Depth_m)

depths <- rbind(boundary_points, sb.sf) %>%
  cbind(., st_coordinates(.)) %>% 
  filter(!is.na(Depth_m)) %>% 
  mutate(X = round(X,-1)) %>% 
  mutate(Y = round(Y,-1)) %>% 
  group_by(X, Y) %>% 
  summarise(Depth_m = mean(Depth_m), 
            geometry = st_centroid(st_union(geometry)))

#remove point outside
depths = depths %>% 
  filter(!(X == 566860 & Y == 286480)) %>% 
  filter(!(X == 566510 & Y == 286340)) %>% 
  filter(!(X == 566440 & Y == 287040)) %>% 
  filter(X < 568240) %>% 
  filter(Y < 287530)

p1 = ggplot(depths) +
  geom_sf(aes(x = X, y = Y, col = Depth_m)) +
  scale_color_viridis_c(); p1

# Check for duplicates
depths %>% 
  group_by(X, Y) %>%
  mutate(dupe = n() > 1) %>%
  filter(dupe == TRUE)

# Write shapefile of depths
st_write(depths, 'data/map/DNR_pointsurvey/pointsDepths.shp', delete_dsn = TRUE)

#  Need a similar object for the regularly-spaced output that we will use to create a raster at the end
grid <- st_make_grid(boundary, n = 100, what = "centers") %>%
  st_as_sf() %>% 
  # filter(st_contains(boundary, ., sparse = FALSE)) %>%
  # cbind(., st_coordinates(.))
  st_as_sf() %>%
  filter(lengths(st_contains(boundary, .)) > 0) %>% 
  cbind(., st_coordinates(.))

ggplot(grid) + 
  geom_sf(data = boundary, fill = 'blue') +
  geom_sf()

# Inverse Distance Weighting (IDW)
fit_gstat <- gstat::gstat(
  formula = Depth_m ~ 1,
  data = as(depths, "Spatial"),
  nmax = 20, nmin = 2,
  set = list(idp = 1.5)
)

grid$IDW <- predict(fit_gstat, newdata = as(grid, "Spatial")) %>%
  st_as_sf() %>%
  pull(1)

# Thin Plate Regression Spline (TPRS)
library(mgcv)
fit_gam_reml <- mgcv::gam(Depth_m ~ s(X, Y, k = 200), data = depths, method = "REML")
grid$TPRS <- predict(fit_gam_reml, newdata = grid, type = "response")
# grid$TPRS = grid$IDW

# Contouring TPRS ####
# Convert grid to raster 
depth_raster <- grid %>% 
  st_set_geometry(NULL) %>% 
  select(X, Y, TPRS) %>% 
  raster::rasterFromXYZ(crs = raster::crs("+init=epsg:4326"))

# Contour lines
depth_contours <- depth_raster %>% 
  raster::rasterToContour(levels = seq(0.5, 4, by = 0.5)) %>% 
  st_as_sf()

# Polygons of contours
pols = list()
pols[[1]] = boundary %>% select(!everything()) %>% mutate(TPRS = 0) %>% 
  mutate(Depth_m = 0)

for (i in 1:8) {
  usedepth = seq(0.5, 4, by = 0.5)[i]
  depth_raster <- grid %>% 
    st_set_geometry(NULL) %>% 
    select(X, Y, TPRS) %>%
    mutate(TPRS = ifelse(TPRS <= usedepth, NA, usedepth))  %>% 
    raster::rasterFromXYZ(crs = raster::crs("+init=epsg:3071"))
    # raster::rasterFromXYZ(crs = st_crs(boundary))
  
  r_df <- raster::as.data.frame(depth_raster, xy = TRUE)  # adds x and y columns
  # ggplot(r_df, aes(x = x, y = y, fill = TPRS)) +
  #   geom_tile() +
  #   coord_equal() +
  #   scale_fill_viridis_c(option = "magma") +
  #   theme_minimal() +
  #   labs(title = usedepth, fill = "Depth (m)")
  
  r <- rast(depth_raster)          # convert from raster to terra
  poly <- as.polygons(r, dissolve = TRUE)
  
  pols[[i+1]] <- st_as_sf(poly) %>% 
    smoothr::smooth(method = "ksmooth", smoothness = 2) %>% 
    mutate(Depth_m = usedepth)
  
  # ggplot(pols[[i+1]]) + geom_sf()
  
}
pols.sf <- do.call(rbind, pols)

# Write shapefile of contours
# st_write(pols.sf, 'SSB_contours.shp')

# Plotting Bathymetry TPRS ####
bathy.tprs = ggplot(grid) +
  geom_sf(data = boundary) +
  geom_raster(aes(X, Y, fill = TPRS)) +
  # geom_sf(data = pols.sf) +
  geom_sf(data = depth_contours) +
  scale_fill_viridis_c() +
  annotation_scale(location = "br") +
  theme_bw(base_size = 9) +
  labs(title = 'TPRS grid', x = NULL, y = NULL, fill = "Depth (m)"); bathy.tprs

bathy.tprs.poly = ggplot(pols.sf) +
  geom_sf(aes(fill = factor(Depth_m))) +
  # geom_sf(data = grid) +
  scale_fill_viridis_d() +
  annotation_scale(location = "br") +
  theme_bw(base_size = 9) +
  labs(title = 'Depth_m contours', x = NULL, y = NULL, fill = "Depth (m)"); bathy.tprs.poly

# Side by side bathymetry
p1 / bathy.tprs.poly
# ggsave('SSB_bathymetry_TPRS.png', width = 10, height = 4)

pols.sf.simple = pols.sf %>% 
  st_collection_extract("POLYGON") %>%  # pulls out all polygon pieces
  st_cast("POLYGON")  %>%                   # then splits multipolygons into rows
  st_simplify(preserveTopology = TRUE, 5) 
st_write(pols.sf.simple, 'data/map/DNR_pointsurvey/DNR_bathymetry.shp', delete_dsn = TRUE)

# Lake Volume and Mean Depth ####
boundary_area <- st_area(boundary) %>% 
  as.numeric()

grid %>% 
  st_set_geometry(NULL) %>% 
  summarise(
    mean_depth = mean(TPRS),
    volume = mean(TPRS) * boundary_area)

# Lake Hyposometry ####
# hyposometry = data.frame(Depth = seq(0,3.5, by = 0.5), Area_m2 = st_area(pols.sf))
# write_csv(hyposometry, 'Wingra_hypsometry.csv')




