# LTER Zooplankton Density

# Libraries 
library(tidyverse)
library(lubridate)

# Load in datasets #=======================
# Package ID: knb-lter-ntl.90.36 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Zooplankton - Madison Lakes Area 1997 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/36/5880c7ba184589e239aec9c55f9d313b" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"'))

dt1 <- read_csv(infile1) |> 
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

# conversion <- read_csv('data/Mass_Calc_LTER_v2.csv') |> 
#   mutate(species = tolower(species)) |> 
#   mutate(species = case_when(species == 'daphnia galeata mendotae' ~ 'daphnia mendotae',
#                              species == 'daphnia pulex' ~ 'daphnia pulicaria',
#                              species == 'acanthocyclops vernalis' ~ 'acanthocyclops',
#                              species == 'tropocyclops prasinus mexicanus' ~ 'tropocyclops',
#                              TRUE ~ species))

# unique(zoops$species_name)[!unique(zoops$species_name) %in% unique(conversion$species)]

# Join zoop data with conversion file 
zoops_join = dt1 |>
  # left_join(conversion, by = join_by('species_name' == 'species')) |>
  # mutate(biomass_ugL = mass_ug * density) |>
  mutate(group = case_when(
    species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus", "tropocyclops", "diacyclops thomasi") ~ "Copepoda",
    species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes", "diaptomid", "calanoid") ~ "Copepoda",
    species_name %in% c("daphnia pulicaria", "daphnia retrocurva", "daphnia mendotae", "daphnia parvula", "diaphanosoma birgei", "daphnia") ~ "Daphnia",
    species_name %in% c("bosmina", "chydorus", "sinobosmina fryei") ~ "Branchiopoda",
    species_name %in% c("copepod nauplii", "copepodites") ~ "Small Cladocera",
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

zoops_all = zoops_join |> filter(month(sample_date) %in% c(6,7,8)) |> 
  group_by(sample_date, group) |> 
  summarise(density = sum(density, na.rm = T)) |> 
  rename(sampledate = sample_date)

zoops_year = zoops_all |> 
  group_by(year = year(sampledate), group) |> 
  summarise(density = mean(density, na.rm = T))

ggplot(zoops_year) +
  geom_point(aes(x = year, density)) +
  facet_wrap(~group)


a = zoops_all |> left_join(chloro_all)
ggplot(a) +
  geom_point(aes(x = density, y = chl_use)) +
  facet_wrap(~group, scales = 'free')

