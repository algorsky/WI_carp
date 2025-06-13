# LTER Zooplankton Length -> Biomass # 

# Libraries 
library(tidyverse)
library(lubridate)
library(here)
remotes::install_github("bmcafee/EDIutilsAddons")
library(EDIutilsAddons)


# Load in datasets #=======================
zoops = get_data('knb-lter-ntl.90.33')
zoops

conversion = read_csv('data/zoops/conversion_alpha-beta.csv')
conversion

# Join datasets #========================
zoops$species_name = tolower(zoops$species_name)
zoops

conversion$species = tolower(conversion$species)
conversion

conversion_join = conversion %>% 
  select(larger_group, species,  length,  mass_formula, mass_formula_reference,alpha, alpha2, beta, modifier, exp, mod, base, tricho) %>% 
  rename(species_name = species, 
         length_mm_ref = length)
conversion_join

# Separate by lake to make a little more feasible and trim down info # =====================

## Wingra ##============================
zoops_WI = zoops %>%
  filter(lakeid == 'WI') %>% 
  select(!c(lakeid, year4, station, species_code)) 
# Remove lakeid - R object is coded to specific lake 
# Remove year4 - can use lubridate to extract that out from sample_date 
# remove station as these are deep hole sites + it's reflected in tow depth 
# remove species code as we have species name 
zoops_WI

zoops_WI_conv = left_join(zoops_WI, conversion_join, by = 'species_name')
zoops_WI_conv

### Lots of NAs for species that don't have an alpha or beta value ##================================= 
findmissing_WI = zoops_WI_conv %>% 
  filter(is.na(alpha))
findmissing_WI
missing.spp_WI = unique(findmissing_WI$species_name) # 3 missing species 
# "leptodora kindti"         "bythotrephes longimanus " "ceriodaphnia dubia"     
missing.spp_WI

#Remove spinies, chaobs, and leptodora as there's better ways to sample for those (which exist in LTER data) 
zoops_WI_conv = filter(zoops_WI_conv, species_name != "bythotrephes longimanus " & species_name != 'ceriodaphnia dubia' & 
                         species_name != 'leptodora kindti')
zoops_WI_conv
unique(zoops_WI_conv$species_name)
### find species that don't have an average length listed from the conversions data frame ##======================= 
findmissing_WI.length = zoops_WI_conv %>% 
  filter(is.na(avg_length)) 
findmissing_WI.length	

missing.length_WI = unique(findmissing_WI.length$species_name) 
missing.length_WI # 15 species # 

# copepod nauplii; diacyclops thomasi;"diaptomid";"mesocyclops edax";"daphnia";
#"acanthocyclops";"tropocyclops";"daphnia pulicaria";"bythotrephes longimanus";
#"tropocyclops prasinus mexicanus"; "copepodites"      

### Find the annual average for that species in the dataset ##===========================
# Use monona when there are no values for Wingra
zoops_MO = zoops %>%
  filter(lakeid == 'MO') %>% 
  select(!c(lakeid, year4, station, species_code)) 
# Remove lakeid - R object is coded to specific lake 
# Remove year4 - can use lubridate to extract that out from sample_date 
# remove station as these are deep hole sites + it's reflected in tow depth 
# remove species code as we have species name 
zoops_MO

zoops_MO_conv = left_join(zoops_MO, conversion_join, by = 'species_name')
zoops_MO_conv
# copepod nauplii
copepod.nauplii_range = zoops_MO_conv %>% 
  filter(species_name == 'copepod nauplii') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
copepod.nauplii_range
copepod.nauplii_annum = copepod.nauplii_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
copepod.nauplii_annum

# diacyclops thomasi 
diacyclops.thomasi_range = zoops_WI_conv %>% 
  filter(species_name == 'diacyclops thomasi') %>%
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
diacyclops.thomasi_range
diacyclops.thomasi_annum = diacyclops.thomasi_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
diacyclops.thomasi_annum

# mesocyclops edax 
mesocyclops.edax_range = zoops_MO_conv %>% 
  filter(species_name == 'mesocyclops edax') %>%
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>%
  ungroup()
mesocyclops.edax_range
mesocyclops.edax_annum = mesocyclops.edax_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
mesocyclops.edax_annum

# daphnia 
daphnia_range = zoops_WI_conv %>% 
  filter(species_name == 'daphnia') %>%
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
daphnia_range 
daphnia_annum = daphnia_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
daphnia_annum

# daphnia pulicaria
daphnia.pulicaria_range = zoops_WI_conv %>%
  filter(species_name == 'daphnia pulicaria') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
daphnia.pulicaria_range
daphnia.pulicaria_annum = daphnia.pulicaria_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.pulicaria_annum

# diaptomid
diaptomid_range = zoops_WI_conv %>% 
  filter(species_name == 'diaptomid') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
diaptomid_range
diaptomid_annum = diaptomid_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
diaptomid_annum

# acanthocyclops
acanthocyclops_range = zoops_WI_conv %>% 
  filter(species_name == 'acanthocyclops') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
acanthocyclops_range
acanthocyclops_annum = acanthocyclops_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
acanthocyclops_annum  

# tropocyclops
tropocyclops_range = zoops_ME_conv %>% 
  filter(species_name == 'tropocyclops') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
tropocyclops_range
tropocyclops_annum = tropocyclops_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
tropocyclops_annum

# bythotrephes.longimanus
bythotrephes.longimanus_range = zoops_WI_conv %>% 
  filter(species_name == 'acanthocyclops') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
bythotrephes.longimanus_range
bythotrephes.longimanus_annum = bythotrephes.longimanus_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
bythotrephes.longimanus_annum

# tropocyclops prasinus mexicanus 
tropocyclops.prasinus.mexicanus_range = zoops_MO_conv %>% 
  filter(species_name == 'tropocyclops prasinus mexicanus') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup() %>% 
  summarize(avg_length = mean(avg_length, na.rm = T))
tropocyclops.prasinus.mexicanus_range
tropocyclops.prasinus.mexicanus_annum = tropocyclops.prasinus.mexicanus_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
tropocyclops.prasinus.mexicanus_annum

# tropocyclops
tropocyclops_range = zoops_WI_conv %>% 
  filter(species_name == 'tropocyclops') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
tropocyclops_range # never measured #

# copepodites
copepodites_range = zoops_WI_conv %>% 
  filter(species_name == 'copepodites') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
copepodites_range
copepodites_annum = copepodites_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
copepodites_annum

# leptodora kindti
leptodora.kindti_range = zoops_WI_conv %>% 
  filter(species_name == 'leptodora kindti') %>% 
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
leptodora.kindti_range
leptodora.kindti_annum = leptodora.kindti_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
leptodora.kindti_annum


### Fill in NA length with the global average mean for that species within the dataset ##=============================== 
unique(findmissing_WI.length$species_name)

# this replaces all NA forthe species specified in species_name # 
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == "copepod nauplii"  )] <- copepod.nauplii_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'daphnia')] <- daphnia_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'diacyclops thomasi')] <- diacyclops.thomasi_annum$avg_length
# use the tropocyclops parsinus mexicanus information since tropocyclops not measured 
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'tropocyclops')] <- tropocyclops.prasinus.mexicanus_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'diaptomid')] <- diaptomid_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'daphnia pulicaria')] <- daphnia.pulicaria_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'tropocyclops prasinus mexicanus')] <- tropocyclops.prasinus.mexicanus_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'copepodites')] <- copepodites_annum$avg_length 
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'diaptomid')] <- diaptomid_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'bythotrephes longimanus')] <- bythotrephes.longimanus_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'acanthocyclops')] <- acanthocyclops_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'mesocyclops edax')] <- mesocyclops.edax_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'leptodora kindti')] <- leptodora.kindti_annum$avg_length
zoops_WI_conv

### Now can calculate biomass in ug #=================================
# Equations follow a few different forms denoted by four grouping columns
# exp = ln(alpha) + beta*ln(length) 
# base = alpha*(length)^beta 
# trich = alpha*(length)*((alpha2*length))^2
# mod = (alpha*(length)^beta)*100

# take the avg_length (in mm) and quantify average biomass of that species, 
# then multiply by density to get ug/m2, 
# then divide by tow depth to get ug/m3 (LTER has some reservations about this step - they like hypsometrically pooled data better, I think it's fine)
# then divide by 1000 to get ug/L 

zoops_WI_biomass = zoops_WI_conv %>% 
  mutate(ug = case_when(base == 'Y' ~ (avg_length^beta)*alpha, 
                        exp == 'Y' ~ exp(alpha + beta*log(avg_length)),
                        mod == 'Y' ~ ((avg_length^beta)*alpha)*modifier, 
                        tricho == 'Y' ~ (alpha*avg_length*((alpha2*avg_length))^beta)*modifier
  ))
zoops_WI_biomass

# Only missing species is bythotrephes which for the life of me I can't filter out # 
zoops_WI_biomass = filter(zoops_WI_biomass, species_name != "bythotrephes longimanus " | species_name != "ceriodaphnia dubia")
findmiss.ug = zoops_WI_biomass %>%
  filter(is.na(ug))
unique(findmiss.ug$species_name)	# ITS STILL THERE (whatever)

### Create Mendota dataset with biomass density #=========================
zoops.WI_biomassdens = zoops_WI_biomass %>%
  mutate(ug_m2 = ug*density) %>%
  mutate(ug_m3 = ug_m2/towdepth) %>% # m2 to m3 
  mutate(ug_L = ug_m3/1000) %>% # m3 to L
  select(sample_date, species_name, larger_group, density, avg_length, individuals_measured, ug_m2, ug_m3, ug_L) %>%
  arrange(sample_date)
zoops.WI_biomassdens	

#write_csv(zoops.WI_biomassdens, "data/zoops.biomass.TB.csv")

zoop_biomass<- zoops.WI_biomassdens|>
  mutate(mg_m3 = ug_m3/1000)


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
  summarize(mean_sum = mean(sum),
            n = n())%>%
  mutate(removal = ifelse(year < 2008, "<2008", ">=2008"))


zoops_biomass_group<- zoop_biomass%>%
  mutate(species_name = ifelse(species_name %in% "aglaodiaptomus clavipes", "diaptomus spp", species_name))%>%
  mutate(species = ifelse(species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus", "chydorus"),
                          "Cyclopoid",
                          ifelse(species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes", "diacyclops thomasi", "diaptomid"),
                                 "Calenoid",
                                 ifelse(species_name %in% c("daphnia pulicaria", "daphnia retrocurva", "daphnia mendotae", "daphnia parvula", "diaphanosoma birgei", "daphnia"),
                                        "Daphnia",
                                        ifelse(species_name %in% c("sinobosmina fryei", "copepod nauplii", "copepodites"),
                                               "Small Cladocera",
                                               "Other")))))

zoops_density_group<- zoops_biomass_group%>%
  group_by(sample_date, species_name)%>%
  summarize(biomass = mean(mg_m3), .groups = 'drop')%>%
  ungroup()%>%
  mutate(species = ifelse(species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus",  "diacyclops thomasi","tropocyclops"),
                          "Cyclopoid",
                          ifelse(species_name %in% "copepod nauplii", 
                                 "Nauplii",
                                 ifelse(species_name %in% c("copepodites"), 
                                        "Copepoda",
                                        ifelse(species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes",  "diaptomid"),
                                               "Calanoid",
                                               ifelse(species_name %in% c("daphnia pulicaria",  "daphnia retrocurva", "daphnia mendotae", "daphnia parvula",  "daphnia"),
                                                      "Daphnia",
                                                      ifelse(species_name %in% c("sinobosmina fryei",  "chydorus", "diaphanosoma birgei", "ceriodaphnia dubia"),
                                                             "Small Cladocera",
                                                             "Other")))))))%>%
  group_by(sample_date, species)%>%
  summarize(biomass = sum(biomass))

zoops_density_group$species <- factor(zoops_density_group$species, levels = c("Calanoid",  "Cyclopoid",  "Nauplii", "Copepoda", "Daphnia", "Small Cladocera"))

zoop_timeseries_plot<-ggplot(zoops_density_group, aes(x = sample_date, y = biomass, fill = species)) +
  geom_area(position = 'stack') +
  ylab(expression(paste("Zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c( "#762a83","#af8dc3",  "#e7d4e8", "#f7f7f4", "#008837", "#a6dba0"))+
  guides(fill = guide_legend(
    label = c("Calanoid", "Cyclopoid", "Nauplii", "Copepoda", 
              expression(paste("Large Cladocera (", italic("Daphnia"), ")")), 
              "Small Cladocera")))+
  theme_bw(base_size = 14)+
  theme(legend.title= element_blank())



