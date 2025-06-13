library(tidyverse)

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

arb.precip<- arb.spring|> select(year4, arb.precip)

fil_algae_summary<- fil_algae_timeseries |> select(year4, fil_algae_sum)

summary_medians<- secchi_median%>%
  left_join(tn_median, by = "year4") |>
  left_join(tp_median, by = "year4") |>
  left_join(chloro_median, by = "year4")|>
  left_join(colonization, by = "year4")|>
  left_join(zoop_summer_sum, by = "year4")|>
  left_join(fil_algae_summary, by = "year4")|>
  left_join(arb.precip, by = "year4")

#write_csv(summary_medians, "data/summary_medians.csv")
