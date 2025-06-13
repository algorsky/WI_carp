# library(feather)
library(tidyverse)
library(lubridate)
# library(fuzzyjoin)

# read in output
Wingra <- read_csv('Data/LimnoSat_Wingra.csv')
# Yearly observations
ggplot(Wingra, aes(x = year)) + geom_bar() + labs(y = 'Number of Observations', title = 'Yearly Observations') + theme_bw()

# Monthly observations
Wingra %>% mutate(month = month(date, label = T)) %>%
  ggplot(., aes(x = month)) + geom_bar() + labs(y = 'Number of Observations', title = 'Monthly Observations') + theme_bw()

#Connect dWL to the forel ule index for visualization
fui.lookup <- tibble(dWL = c(471:583), fui = NA)
fui.lookup$fui[fui.lookup$dWL <= 583] = 21
fui.lookup$fui[fui.lookup$dWL <= 581] = 20
fui.lookup$fui[fui.lookup$dWL <= 579] = 19
fui.lookup$fui[fui.lookup$dWL <= 577] = 18
fui.lookup$fui[fui.lookup$dWL <= 575] = 17
fui.lookup$fui[fui.lookup$dWL <= 573] = 16
fui.lookup$fui[fui.lookup$dWL <= 571] = 15
fui.lookup$fui[fui.lookup$dWL <= 570] = 14
fui.lookup$fui[fui.lookup$dWL <= 569] = 13
fui.lookup$fui[fui.lookup$dWL <= 568] = 12
fui.lookup$fui[fui.lookup$dWL <= 567] = 11
fui.lookup$fui[fui.lookup$dWL <= 564] = 10
fui.lookup$fui[fui.lookup$dWL <= 559] = 9
fui.lookup$fui[fui.lookup$dWL <= 549] = 8
fui.lookup$fui[fui.lookup$dWL <= 530] = 7
fui.lookup$fui[fui.lookup$dWL <= 509] = 6
fui.lookup$fui[fui.lookup$dWL <= 495] = 5
fui.lookup$fui[fui.lookup$dWL <= 489] = 4
fui.lookup$fui[fui.lookup$dWL <= 485] = 3
fui.lookup$fui[fui.lookup$dWL <= 480] = 2
fui.lookup$fui[fui.lookup$dWL <= 475 & fui.lookup$dWL >470] = 1

# Actual Forel-Ule Colors
fui.colors <- c(
  "#2158bc", "#316dc5", "#327cbb", "#4b80a0", "#568f96", "#6d9298", "#698c86", 
  "#759e72", "#7ba654", "#7dae38", "#94b660","#94b660", "#a5bc76", "#aab86d", 
  "#adb55f", "#a8a965", "#ae9f5c", "#b3a053", "#af8a44", "#a46905", "#9f4d04")

Wingra <- Wingra %>% left_join(fui.lookup) 

min.fui <- min(Wingra$fui)
max.fui <- max(Wingra$fui)

# Overall Color Distribution
Wingra %>% group_by(dWL, Hylak_id) %>%
  summarise(count = n()) %>%
  left_join(fui.lookup) %>%
  ggplot(., aes(x = dWL, y = count, fill = fui)) + 
  geom_col() +
  scale_fill_gradientn(colours = fui.colors[min.fui:max.fui]) +
  labs(x = 'Wavelength (nm)', title = 'Overall Color Distribution') +
  theme_bw() +
  theme(legend.position = 'none') +
  facet_wrap(vars(Hylak_id))

# Monthly Climatology
ggplot(Wingra, aes(x = month, y = dWL)) + 
  #geom_violin(draw_quantiles = .5) +
  geom_boxplot(outlier.colour = 'transparent') +
  geom_jitter(aes(color = fui), size = 2, position = position_jitter(.2)) +
  scale_color_gradientn(colours = fui.colors[min.fui:max.fui]) +
  labs(y = 'Wavelength (nm)', title = 'Monthly Climatology') +
  theme_bw() +
  theme(legend.position = 'none') +
  facet_wrap(vars(Hylak_id))

# Season color observations over time
Wingra = Wingra |> mutate(
  season = case_when(month %in% c('Jun', 'Jul', 'Aug') ~ 'Summer (jja)',
                     month %in% c('Apr', 'May') ~ 'Spring (am)',
                     month %in% c('Sep', 'Oct') ~ 'Fall (so)')) |> 
  mutate(season = factor(season, levels = c('Spring (am)', 'Summer (jja)', 'Fall (so)'))) |> 
  filter(year >= 2000)

Wingra |> 
  filter(!is.na(season)) |> 
  ggplot(aes(x = date, y = dWL)) + 
  geom_point(aes(color = fui), size = 3) +
  # geom_smooth(se = T, method = 'lm') +
  scale_color_gradientn(colours = fui.colors[min.fui:max.fui]) +
  labs(y = 'Dominant Wavelength (nm)', x = 'Year', title = 'Lake Wingra') +
  theme_bw() +
  theme(legend.position = 'none') +
  facet_wrap(~season)

limnosat = Wingra |> 
  filter(month(date) %in% c(6,7,8)) |> 
  select(sampledate = date, dWL, fui) |> 
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

