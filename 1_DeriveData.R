library(tidyverse)
# devtools::install_github('hdugan/NTLlakeloads')
library(NTLlakeloads)

#################### Secchi ####################
secchi = loadLTERsecchi() |> filter(lakeid == 'WI') |> 
  select(sampledate, year4, secnview) |>
  filter(month(sampledate) %in% c(6,7,8)) |>
  filter(year4 < 2024) |>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

#################### Nutrients ####################
nuts = loadLTERnutrients() |> filter(lakeid == 'WI')

# Total unfiltered nitrogen
tn <- nuts |> 
  filter(month(sampledate) %in% c(6,7,8)) |> 
  filter(depth == 0) |> 
  select(sampledate, year4, lakeid, totnuf_WSLH, totnuf) |> 
  mutate(totnuf_WSLH = totnuf_WSLH * 1000) |> 
  pivot_longer(cols = c(totnuf_WSLH, totnuf)) |> 
  group_by(sampledate) |>
  summarise(totnuf = mean(value, na.rm = T)) |> 
  na.omit()|>
  filter(totnuf > 12)|>
  mutate(year4 = year(sampledate))|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))
  # filter(year4 != 2020)

ggplot(tn) +
  geom_point(aes(x = sampledate, y = totnuf))

# Total unfiltered phosphorus 
tp <- nuts |> 
  filter(month(sampledate) %in% c(6,7,8)) |> 
  filter(depth == 0) |> 
  select(sampledate, year4, lakeid, totpuf_WSLH, totpuf) |> 
  mutate(totpuf_WSLH = totpuf_WSLH * 1000) |> 
  pivot_longer(cols = c(totpuf_WSLH, totpuf)) |> 
  filter(value < 400) %>%
  group_by(sampledate) |>
  summarise(totpuf = mean(value, na.rm = T)) |> 
  mutate(year4 = year(sampledate))|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))
  # filter(year4 != 2020)

ggplot(tp) +
  geom_point(aes(x = sampledate, y = totpuf))

#################### Chlorophyll ####################
source('0_chlorophyll.R')

#################### DNR Macrophyte ####################
macrophyte_dnr <- read_csv("data/dnr_macrophyte_sum.csv")

macrophyte_timeseries<- macrophyte_dnr|>mutate(removal = ifelse(Year < 2008, "<2008", ">=2008"))

colonization <- macrophyte_dnr %>%
  select(Year, `Maximum depth of plants (ft)`) %>%
  rename(colonization_ft = `Maximum depth of plants (ft)`) %>%
  rename(year4 = Year) %>%
  mutate(colonization_m = colonization_ft*0.3048)

#################### Zooplankton ####################
zoop_biomass<- read_csv("data/zoops.biomass.TB.csv")|>
  mutate(mg_m3 = ug_m3/1000)

#################### Filamentous algae ####################
source('0_filalgae.R')

#################### Precipitation ####################
# Get arboretum .csv from github repository
arb.precip = read_csv('https://raw.githubusercontent.com/hdugan/Wingra_SaltTrajectory/refs/heads/main/data_input/Climate/3944435.csv') |> 
  mutate(year4 = year(DATE))

arb.spring <- arb.precip |>
  filter(NAME == 'UW ARBORETUM MADISON, WI US') |> 
  filter(year4 >= 1995) |> 
  filter(month(DATE) %in% c(3,4,5,6,7,8)) |> 
  group_by(year4, NAME) |>
  summarise(arb.precip = sum(PRCP, na.rm = T), num_na = sum(is.na(PRCP))) |> 
  select(year4, arb.precip)

#################### LimnoSat ####################
source('0_limnosat.R')

#################### Summary medians ####################

secchi_median = secchi |> 
  group_by(year4) |> 
  summarise(median_secchi = median(secnview, na.rm = TRUE))

tn_median = tn |> 
  group_by(year4) |> 
  summarise(median_totnuf = median(totnuf, na.rm = TRUE))

tp_median = tp |> 
  group_by(year4) |> 
  summarise(median_totpuf = median(totpuf, na.rm = TRUE))

chloro_median = chloro_all |> 
  group_by(year4) |> 
  summarise(median_chla = median(chl_use, na.rm = TRUE))

limnosat_median = limnosat |> 
  group_by(year4 = year(sampledate)) |> 
  summarise(dWL = median(dWL, na.rm = TRUE))

summary_medians <- secchi_median %>%
  left_join(tn_median, by = "year4") |>
  left_join(tp_median, by = "year4") |>
  left_join(chloro_median, by = "year4")|>
  left_join(colonization, by = "year4")|>
  # left_join(zoop_summer_sum, by = "year4")|>
  left_join(fil_algae_timeseries |> select(year4, fil_algae_sum), by = "year4")|>
  left_join(arb.spring, by = "year4") |> 
  left_join(limnosat_median, by = 'year4') |> 
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))
