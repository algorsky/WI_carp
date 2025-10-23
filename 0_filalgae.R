library(tidyverse)
library(ggpattern)
# Download data from EDI
# North Temperate Lakes LTER: Macrophyte Biomass - Madison Lakes Area 1995 - current
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/24/32/a03d18be68db4cf2280846afe2643d5e" 

infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

macrophyte_ntl <- read_csv(infile1) |> filter(lakeid == "WI") %>% 
  mutate(fil_algae_wt = if_else(year4 == 2008, fil_algae_wt * 10, fil_algae_wt)) %>% 
  mutate(plant_wt_hand = if_else(year4 == 2008, plant_wt_hand * 10, plant_wt_hand))

#Filamentous algae
# fil_algae_timeseries <- macrophyte_ntl %>%
#   mutate(fil_algae_wt = replace_na(fil_algae_wt, 0)) %>%
#   filter(transect %in% c(2,5,7,9,11)) |> 
#   group_by(year4) %>%
#   summarize(fil_algae_mean = mean(fil_algae_wt, na.rm = T), 
#             plant_wt_mean = mean(plant_wt_hand, na.rm = T)) %>%
#   mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

# Read in shapefile 
bathy = st_read('data/map/Wingra_bathymetry_2025.shp') %>% 
  mutate(area = st_area(.)) %>% 
  group_by(Depth_m) %>% 
  summarise(area = sum(area)) %>% 
  st_drop_geometry() %>% 
  arrange(area) %>% 
  filter(Depth_m %in% c(0,1,2,3)) %>% 
  mutate(area_diff = c(area[1], diff(area))) %>% 
  mutate(area_per = as.numeric(area/sum(area))) %>% 
  rename(depth = Depth_m) 

# Convert data to area depth
benthic_spatial = macrophyte_ntl %>%
  mutate(fil_algae_wt = replace_na(fil_algae_wt, 0)) %>%
  filter(transect %in% c(2,5,7,9,11)) %>% 
  mutate(depth = floor(depth)) %>%
  group_by(year4, depth) %>% 
  summarise(
    across(where(~ is.numeric(.) | inherits(., "Date")), 
      ~ mean(., na.rm = TRUE))) %>% 
  left_join(bathy) %>% 
  mutate(fil_algae_spatial = fil_algae_wt*area_per, plant_wt_spatial = plant_wt_hand*area_per) %>% 
  mutate(depth = factor(depth, levels = c(4,3.5,3,2.5,2,1.5,1,0.5,0)))

benthic_spatial_year = benthic_spatial %>% 
  group_by(year4) %>%
  summarise(fil_algae_spatial = sum(fil_algae_spatial, na.rm = T), 
            plant_wt_spatial = sum(plant_wt_spatial, na.rm = T)) %>% 
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

#write_csv(fil_algae_timeseries, "data/fil_algae_timeseries.csv")

# p1 = ggplot(fil_algae_timeseries) +
#   geom_col(aes(x = year4, y = fil_algae_mean))
# 
# p2 = ggplot(fil_algae_timeseries) +
#   geom_col(aes(x = year4, y = plant_wt_mean), fill = 'darkgreen')
# p1/p2
# 
p1 = ggplot(benthic_spatial) +
  geom_col(aes(x = year4, y = fil_algae_spatial, fill = as.factor(depth)))

p2 = ggplot(benthic_spatial, aes(x = year4, y = plant_wt_spatial,
                                 fill = as.factor(depth))) +
  geom_col();p2

p1/p2 + plot_layout(guides = 'collect')

# Stats for paper 
fil_algae_timeseries |> filter(year4 > 2008) |> summarise(min(fil_algae_mean), max(fil_algae_mean), sd(fil_algae_mean))

