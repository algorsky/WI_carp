
library(tidyverse)
library(MetBrewer)

df = read_csv('~/Downloads/ntl24_v8.csv') |> 
  filter(lakeid == 'WI') |> 
  filter(transect %in% c(2,5,7,9,11)) |> 
  group_by(lakeid, year4, sampledate, transect, depth) |> 
  summarise_all(mean, na.rm = T)

ggplot(df) +
  geom_point(aes(x = sampledate, y = depth, col = log(plant_wt_hand), size = log(plant_wt_hand)), shape = 15) +
  facet_wrap(~transect, ncol = 1) +
  scale_size_continuous(range = c(1,3)) +
  scale_y_reverse() +
  scale_color_gradientn(colors = met.brewer("VanGogh1")) +
  theme_bw() +
  ylab('Depth (m)') +
  theme(axis.title.x = element_blank())
