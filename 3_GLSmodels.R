library(tidyverse)
library(nlme)

ag.data<- read_csv("data/summary_medians.csv")|> 
  mutate(group = if_else(year4 < 2008, 'pre', 'post'))

#Secchi 
acf(ag.data$median_secchi)
#YES

#TN
tn_gls<- ag.data%>%
  select(median_totnuf, group, year4)%>%
  na.omit()
acf(tn_gls$median_totnuf)
#YES

#TP
tp_gls<- ag.data%>%
  select(median_totpuf, group, year4)%>%
  na.omit()
acf(tp_gls$median_totpuf)
#YES

#Chlorophyll
chl_gls<- ag.data%>%
 select(median_chla, group)%>%
  na.omit()
acf(chl_gls$median_chla)
#NO

#filamentous algae
fil_gls<- ag.data%>%
  select(fil_algae_sum, group, year4)%>%
  na.omit()
acf(fil_gls$fil_algae_sum)
#NO

#zooplankton
zoop_gls<-ag.data %>%
  select(zoop_summer_mgm3, group, year4)%>%
  na.omit()
acf(zoop_gls$zoop_summer_mgm3)
#NO

gls_secchi <- gls(median_secchi ~ group, data = ag.data, correlation = corAR1(form = ~ year4))
summary(gls_secchi)
residuals_gls <- resid(gls_secchi) # extract model residuals 
acf(residuals_gls, main = "ACF of GLS Residuals")  # If the AR(1) structure has effectively accounted for autocorrelation, 
# the ACF plot should show little to no significant autocorrelation

gls_tn <-  gls(median_totnuf ~ group, data = tn_gls, correlation = corAR1(form = ~ year4))
summary(gls_tn)

gls_tp <-  gls(median_totpuf ~ group, data = tp_gls, correlation = corAR1(form = ~ year4))
summary(gls_tp)

gls_chloro <- gls(median_chla ~ group, data = chl_gls)
summary(gls_chloro)

gls_zoop <- gls(sum_fil_algae ~ group, data = fil_gls)
summary(gls_zoop)

gls_fil <- gls(fil_algae_sum ~ group, data = fil_gls)
summary(gls_fil)

