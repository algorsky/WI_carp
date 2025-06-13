library(tidyverse)
library(NTLlakeloads)

#Secchi
secchi = loadLTERsecchi() |> filter(lakeid == 'WI') |> 
  select(sampledate, year4, secnview) |>
  filter(month(sampledate) %in% c(6,7,8)) |>
  filter(year4 < 2024) |>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

#Nutrients
nuts = loadLTERnutrients() |> filter(lakeid == 'WI')
tn<- nuts|> 
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
  mutate(group = if_else(year4 < 2008, 'pre', 'post'))%>%
  filter(year4 != 2020)

ggplot(tp)+
  geom_point(aes(x = sampledate, y = totpuf))

tp<- nuts|> 
  filter(month(sampledate) %in% c(6,7,8)) |> 
  filter(depth == 0) |> 
  select(sampledate, year4, lakeid, totpuf_WSLH, totpuf) |> 
  mutate(totpuf_WSLH = totpuf_WSLH * 1000) |> 
  pivot_longer(cols = c(totpuf_WSLH, totpuf)) |> 
  filter(value < 400)%>%
  group_by(sampledate) |>
  summarise(totpuf = mean(value, na.rm = T)) |> 
  mutate(year4 = year(sampledate))|>
  mutate(group = if_else(year4 < 2008, 'pre', 'post'))%>%
  filter(year4 != 2020)


#Chlorophyll from 0_chlorophyll.R
chloro_all<- read_csv("data/chloro_WI.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))


#DNR Macrophyte
macrophyte_dnr<- read_csv("data/dnr_macrophyte_sum.csv")
macrophyte_timeseries<- macrophyte_dnr|>mutate(removal = ifelse(Year < 2008, "<2008", ">=2008"))
colonization<- macrophyte_dnr%>%
  select(Year, `Maximum depth of plants (ft)`)%>%
  rename(colonization_ft = `Maximum depth of plants (ft)`)%>%
  rename(year4 = Year)%>%
  mutate(colonization_m = colonization_ft*0.3048)

#Zooplankton from 0_zooplankton.R
zoop_biomass<- read_csv("data/zoops.biomass.TB.csv")|>
  mutate(mg_m3 = ug_m3/1000)

#Filamentous algae
fil_algae_timeseries<- read_csv("data/fil_algae_timeseries.csv")

#Precip
# Grab .csv from Lizzie's repo 
arb.precip = read_csv('https://raw.githubusercontent.com/hdugan/Wingra_SaltTrajectory/refs/heads/main/data_input/Climate/3944435.csv') |> 
  mutate(year4 = year(DATE))

arb.spring<- arb.precip|>
  filter(NAME == 'UW ARBORETUM MADISON, WI US') |> 
  filter(year4 >= 1995) |> 
  filter(month(DATE) %in% c(3,4,5,6,7,8)) |> 
  group_by(year4, NAME) |>
  summarise(arb.precip = sum(PRCP, na.rm = T), num_na = sum(is.na(PRCP)))


#Summary medians
summary_medians<- read_csv("data/summary_medians.csv")
