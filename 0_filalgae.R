library(tidyverse)
# Download data from EDI
# North Temperate Lakes LTER: Macrophyte Biomass - Madison Lakes Area 1995 - current
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/24/32/a03d18be68db4cf2280846afe2643d5e" 

infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

macrophyte_ntl <- read_csv(infile1) |> filter(lakeid == "WI") 

#Filamentous algae
fil_algae_timeseries <- macrophyte_ntl %>%
  mutate(fil_algae_wt = replace_na(fil_algae_wt, 0)) %>%
  group_by(year4) %>%
  summarize(fil_algae_sum = sum(fil_algae_wt, na.rm = T), plant_wt_sum = sum(plant_wt_hand, na.rm = T)) %>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

#write_csv(fil_algae_timeseries, "data/fil_algae_timeseries.csv")
# 
# p1 = ggplot(fil_algae_timeseries) +
#   geom_col(aes(x = year4, y = fil_algae_sum))
# 
# p2 = ggplot(fil_algae_timeseries) +
#   geom_col(aes(x = year4, y = plant_wt_sum), fill = 'darkgreen')
# p1/p2

# Stats for paper 
fil_algae_timeseries |> filter(year4 > 2008) |> summarise(min(fil_algae_sum), max(fil_algae_sum), sd(fil_algae_sum))
tail(fil_algae_timeseries)
