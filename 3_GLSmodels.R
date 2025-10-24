
library(nlme)

# Test autocorrelation of timseries
acf(summary_means$mean_secchi, na.action = na.pass) #yes
acf(summary_means$mean_totnuf, na.action = na.pass) #yes
acf(summary_means$mean_totpuf, na.action = na.pass) #yes
acf(summary_means$mean_tpm, na.action = na.pass) #no
acf(summary_means$mean_chla, na.action = na.pass) #no
acf(summary_means$plant_wt_spatial, na.action = na.pass) #yes
acf(summary_means$fil_algae_spatial, na.action = na.pass) #no
acf(summary_means$redblue, na.action = na.pass) #yes

# To assess differences in water clarity pre vs. post removal, we
# used a generalized least squares (GLS) model with an autoregressive
# correlation structure. Here, we modeled mean Secchi depth
# (mean_secchi) as a function of (group) using the gls()
# function from the nlme package in R. To account for potential temporal
# autocorrelation in repeated annual measurements, we included a first-order
# autoregressive correlation structure (corAR1) with respect to year (year4).
# This structure models residuals from consecutive years as being correlated,
# thereby improving model estimates when temporal dependence is present.

gls_secchi <- gls(mean_secchi ~ removal, data = summary_means |> filter(!is.na(mean_secchi)), correlation = corAR1(form = ~ year4))
summary(gls_secchi)
residuals_gls <- resid(gls_secchi) # extract model residuals 
acf(residuals_gls, main = "ACF of GLS Residuals")  # If the AR(1) structure has effectively accounted for autocorrelation, 
# the ACF plot should show little to no significant autocorrelation

gls_tn <-  gls(mean_totnuf ~ removal, data = summary_means |> filter(!is.na(mean_totnuf)), correlation = corAR1(form = ~ year4))
# gls_tn <-  gls(mean_totnuf ~ removal, data = summary_means |> filter(!is.na(mean_totnuf)))
summary(gls_tn)

gls_tp <-  gls(mean_totpuf ~ removal, data = summary_means |> filter(!is.na(mean_totpuf)), correlation = corAR1(form = ~ year4))
summary(gls_tp)

gls_tpm <-  gls(mean_tpm ~ removal, data = summary_means |> filter(!is.na(mean_tpm)))
summary(gls_tpm)

gls_chloro <- gls(mean_chla ~ removal, data = summary_means |> filter(!is.na(mean_chla)))
summary(gls_chloro)

gls_RB <- gls(redblue ~ removal, data = summary_means |> filter(!is.na(redblue)), correlation = corAR1(form = ~ year4))
summary(gls_RB)

gls_plant <- gls(plant_wt_spatial ~ removal, data = summary_means |> filter(!is.na(plant_wt_spatial)), correlation = corAR1(form = ~ year4))
summary(gls_plant)

gls_algae <- gls(fil_algae_spatial ~ removal, data = summary_means |> filter(!is.na(fil_algae_spatial)))
summary(gls_algae)

# Means before and after
summary_means |> 
  group_by(removal) |> 
  summarise_all(median, na.rm = T)

