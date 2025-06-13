# LTER Zooplankton Length -> Biomass # 

# Libraries 
library(tidyverse)
library(lubridate)
library(here)
remotes::install_github("bmcafee/EDIutilsAddons")
library(EDIutilsAddons)


# Load in datasets #=======================
# Update species names to match length conversion file 
zoops = get_data('knb-lter-ntl.90.33') |> 
  filter(lakeid == 'WI') |> 
  mutate(species_name = tolower(species_name)) |> 
  mutate(species_name = if_else(str_starts(species_name, "bythotrephes"), "bythotrephes longimanus", species_name)) |> 
  mutate(species_name = if_else(species_name == 'copepodites', "copepod nauplii", species_name)) |> 
  mutate(species_name = if_else(species_name == 'tropocyclops prasinus mexicanus', "tropocyclops", species_name)) |> 
  mutate(species_name = if_else(species_name == 'sinobosmina fryei', "bosmina", species_name)) |> 
  mutate(species_name = if_else(species_name == 'diaptomid', "calanoid", species_name)) |> 
  mutate(species_name = if_else(species_name == 'aglaodiaptomus clavipes', "calanoid", species_name)) |> 
  filter(!species_name %in% c("bythotrephes longimanus", "ceriodaphnia dubia", "leptodora kindti"))

# Package ID: knb-lter-ntl.376.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER Zooplankton conversion formulas length to biomass.
# Had to be downloaded to get embedded excel equations to work

conversion <- read_csv('data/Mass_Calc_LTER_v2.csv') |> 
  mutate(species = tolower(species)) |> 
  mutate(species = case_when(species == 'daphnia galeata mendotae' ~ 'daphnia mendotae',
                             species == 'daphnia pulex' ~ 'daphnia pulicaria',
                             species == 'acanthocyclops vernalis' ~ 'acanthocyclops',
                             species == 'tropocyclops prasinus mexicanus' ~ 'tropocyclops',
                             TRUE ~ species))

unique(zoops$species_name)[!unique(zoops$species_name) %in% unique(conversion$species)]

# Join zoop data with conversion file 
zoops_join = zoops |> left_join(conversion, by = join_by('species_name' == 'species')) |> 
  mutate(biomass_ugL = mass_ug * density) |> 
  mutate(group = case_when(
    species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus", "chydorus", "tropocyclops") ~ "Cyclopoid",
    species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes", "diacyclops thomasi", "diaptomid", "calanoid") ~ "Calanoid",
    species_name %in% c("daphnia pulicaria", "daphnia retrocurva", "daphnia mendotae", "daphnia parvula", "diaphanosoma birgei", "daphnia") ~ "Daphnia",
    species_name %in% c("bosmina") ~ "Bosmina",
    species_name %in% c("sinobosmina fryei", "copepod nauplii", "copepodites") ~ "Small Cladocera",
    TRUE ~ "Other"
  ))

zoops_join |> filter(month(sample_date) %in% c(6,7,8)) |> 
  ggplot() +
  geom_point(aes(x = sample_date, y = avg_length)) +
  facet_wrap(~species_name, scales = 'free_y')

zoops_join |> filter(month(sample_date) %in% c(6,7,8)) |> 
  ggplot() +
  geom_point(aes(x = sample_date, y = density)) +
  facet_wrap(~species_name, scales = 'free_y')


zoops_year = zoops_join |> filter(month(sample_date) %in% c(6,7,8)) |> 
  group_by(sample_date, group) |> 
  summarise(biomass_ugL = sum(biomass_ugL, na.rm = T)) |> 
  group_by(year = year(sample_date), group) |> 
  summarise(mean_biomass_ugL = mean(biomass_ugL, na.rm = T))

ggplot(zoops_year, aes(x = year, y = mean_biomass_ugL/1e6, fill = group)) +
  geom_col() +
  ylab(expression(paste("Zooplankton biomass", " (g ", L^-1,")"))) +
  scale_fill_manual(values = c( "#762a83","#af8dc3",  "#e7d4e8", "#f7f7f4", "#008837", "#a6dba0"))
  

# ggplot(zoops_density_group, aes(x = sample_date, y = biomass_ugL, fill = species)) +
#   geom_area(position = 'stack') +
#   ylab(expression(paste("Zooplankton biomass", " (", µ,"g ", L^-1,")")))+
#   xlab("")+
#   scale_fill_manual(values = c( "#762a83","#af8dc3",  "#e7d4e8", "#f7f7f4", "#008837", "#a6dba0"))+
#   guides(fill = guide_legend(
#     label = c("Calanoid", "Cyclopoid", "Nauplii", "Copepoda", 
#               expression(paste("Large Cladocera (", italic("Daphnia"), ")")), 
#               "Small Cladocera")))+
#   theme_bw(base_size = 14)+
#   theme(legend.title= element_blank())

#zooplankton
summer_zoop_sampling<- zoop_biomass|>
  select(sample_date, mg_m3)%>%
  mutate(month = month(sample_date))%>%
  filter(month > 5 & month < 9)%>%
  mutate(year = year(sample_date))
zoop_summer_mean<- summer_zoop_sampling%>%
  filter(!is.na(mg_m3))%>%
  group_by(sample_date)%>%
  summarize(sum = sum(mg_m3))
zoop_summer_sum<- zoop_summer_mean%>%
  mutate(year = year(sample_date))%>%
  group_by(year)%>%
  summarize(zoop_summer_mgm3 = mean(sum))%>%
  rename(year4 = year)