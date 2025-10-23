library(sf)
library(tidyverse)
library(patchwork)
library(smoothr)
library(cowplot)
library(units)
library(ggrepel)

wingra_bath = st_read('data/map/Wingra_bathymetry_2025.shp') %>%
  mutate(area = st_area(.))
wingra_smooth <- smoothr::smooth(wingra_bath, method = "chaikin") %>%
  mutate(area = st_area(.))

hypsometry = wingra_smooth |> 
  st_drop_geometry() |> 
  group_by(Depth_m) |> 
  summarise(area = sum(area)) |> 
  arrange(area) |> 
  mutate(area.diff = c(min(area), diff(area)))
  
sum(hypsometry$area.diff) # gut check 

# Make bathymetry labels with location 
wingra_edge_labels <- wingra_bath %>% 
  filter(area > set_units(40000, m^2)) %>% 
  mutate(geometry = if_else(Depth_m %in% c(0,1,2,3),
           st_line_sample(st_boundary(geometry), sample = c(0.85)), 
           st_line_sample(st_boundary(geometry), sample = c(0.75)))) |>  # midpoint along edge
  st_as_sf()

ggplot() +
  # geom_sf(data = wingra, fill = "lightblue", color = "blue") +
  geom_sf(data = wingra_smooth |> arrange(Depth_m), fill = 'lightblue') +
  geom_sf_text(
    data = wingra_edge_labels,
    aes(label = Depth_m),
    color = "grey50",
    size = 3
  )


# Starting points of transects
transect_starts_sf <- tribble(
  ~site, ~lat, ~lon,
  "WI2",  43.055987, -89.420747,
  "WI5",  43.055508, -89.409563,
  "WI7",  43.050747, -89.414242,
  "WI9",  43.050468, -89.425120,
  "WI11", 43.051250, -89.431040
) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(wingra_smooth))  # match Wingra CRS

# shore_points <- st_nearest_points(transect_starts, wingra_shore) |>
#   st_cast("POINT")
#   # nearest_points returns start + end point, so we take the second (shoreline) one

# Function to create a transect line from one point, bearing, and length
make_transect <- function(start_point, bearing_deg, length_m) {
  # Convert bearing to radians
  bearing_rad <- bearing_deg * pi / 180
  
  # Calculate endpoint using basic planar geometry
  dx <- length_m * sin(bearing_rad)
  dy <- length_m * cos(bearing_rad)
  # Extract coordinates
  start_coords <- st_coordinates(start_point)
  # Compute end coordinates
  end_coords <- cbind(start_coords[1] + dx, start_coords[2] + dy)
  # Create LINESTRING
  st_sfc(st_linestring(rbind(start_coords, end_coords)), crs = st_crs(start_point))
}

# Example: bearings (degrees from north, clockwise)
bearings <- c(170, 260, 340, 360, 85)  # example values
lengths  <- c(100, 300, 400, 200, 300)  # in meters


# Assuming make_transect returns a LINESTRING as a matrix of coordinates
transects <- pmap(
  list(
    split(transect_starts_sf, seq_len(nrow(transect_starts_sf))),
    bearings,
    lengths
  ),
  \(pt, b, l) make_transect(pt, b, l)
)
# Combine all sfc_LINESTRINGs into one sfc
transects_sfc <- do.call(c, transects)
# Optionally, make it an sf object
transects_sf <- st_sf(geometry = transects_sfc)

################### Map of transects ####################
p1 = ggplot() +
  geom_sf(data = wingra_smooth |> arrange(Depth_m), fill = 'lightblue') +
  geom_sf_text(
    data = wingra_edge_labels,
    aes(label = Depth_m),
    color = "grey20",
    fontface = 'bold',
    size = 1.5
  ) +
  geom_sf(data = transects_sf, color = "red", linewidth = 1) +
  # geom_label()
  geom_sf(data = transect_starts_sf, color = "red", size = 3) +
  geom_sf_text(data = transect_starts_sf, aes(label = site), size = 3, 
               nudge_x = 100, nudge_y = 50) + # labels
  # geom_sf(data = shore_points, color = "red", size = 3) +
  scale_y_continuous(breaks = c(43.05, 43.055)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41)) +
  theme_bw(base_size = 9)+
  theme(
    axis.title = element_blank(),
    legend.position = 'bottom',
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br") +
  theme(aspect.ratio = 0.6); p1  # optional to fix square aspect

####################### Macrophyte data #######################
macro.df = macrophyte_ntl %>%
  mutate(fil_algae_wt = replace_na(fil_algae_wt, 0)) %>%
  mutate(plant_wt_hand = replace_na(plant_wt_hand, 0)) %>%
  filter(transect %in% c(2,5,7,9,11)) |> 
  group_by(year4, transect, depth) %>%
  summarize(fil_algae_sum = mean(fil_algae_wt, na.rm = T), 
            plant_wt_sum = mean(plant_wt_hand, na.rm = T)) %>%
  group_by(year4, depth) %>% 
  summarize(fil_algae_min = min(fil_algae_sum, na.rm = T), 
            fil_algae_max = max(fil_algae_sum, na.rm = T), 
            fil_algae_sum = mean(fil_algae_sum, na.rm = T), 
            plant_wt_min = min(plant_wt_sum, na.rm = TRUE),
            plant_wt_max = max(plant_wt_sum, na.rm = TRUE),
            plant_wt_sum = mean(plant_wt_sum, na.rm = T)) %>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008')) |> 
  filter(depth < 4)

plot_macro_depth <- function(data, usedepths) {
  ggplot(data %>% filter(depth %in% usedepths), aes(x = year4, y = plant_wt_sum)) +
    geom_path(linewidth = 0.2) +
    geom_errorbar(aes(ymin = plant_wt_min, ymax = plant_wt_max), linewidth = 0.2) +
    
    # Filamentous algae on secondary axis
    geom_path(aes(x = year4 + 0.3, y = fil_algae_sum * 10), color = "#1e9478", linewidth = 0.2) +
    geom_errorbar(aes(x = year4 + 0.3, ymin = fil_algae_min * 10, ymax = fil_algae_max * 10), 
                  linewidth = 0.2, color = '#1e9478') +
    
    geom_point(aes(fill = removal), shape = 21, size = 1) + 
    geom_point(aes(x = year4 + 0.3, y = fil_algae_sum * 10), 
               shape = 22, fill = '#1e9478', size = 1) +

    
    scale_y_continuous(
      name = "Macrophyte mean\nbiomass (g wet weight)",
      sec.axis = sec_axis(~./10, name = "Filamentous algae mean\nbiomass (g wet weight)")
    ) +
    
    scale_x_continuous(limits = c(1995, 2025), breaks = seq(1995, 2025, 5)) +
    scale_fill_manual(values = c("white", "grey30")) +
    geom_vline(aes(xintercept = 2008.5), linewidth = 0.3, linetype = 2) +
    
    facet_wrap(~depth, nrow = 2, 
               labeller = labeller(depth = function(x) paste("Depth:", x))) +
    
    theme_bw(base_size = 9) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y.right  = element_text(color = "#1e9478")
    )
}

# Can choose depths to plot
p2 = plot_macro_depth(macro.df, usedepths = c(1,1.5,2,2.5,3,3.5)) +
  theme(legend.position = 'none')
# p3 = plot_macro_depth(macro.df, usedepths = c(2,2.5,3,3.5))


plot_grid(
  p1, p2, ncol = 1,
  rel_heights = c(0.6,1)
)

# Combine vertically
plot_grid(
  p1 + theme(plot.margin = margin(t=5, r=200, b=5, l=0)), p2, 
  ncol = 1, 
  rel_heights = c(0.6, 1),
  labels = c("(a)", "(b)"),
  label_size = 8,
  label_fontface = "plain",
  label_x = 0,   # left-align labels
  label_y = 1    # top of the plot
)

ggsave("figures/FigureS1.png", width = 6.5, height = 5, units = 'in', dpi = 500, 
       bg = 'white')

