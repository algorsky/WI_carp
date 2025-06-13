
library(nlme)

# Test autocorrelation of timseries
acf(summary_medians$median_secchi) #yes
acf(summary_medians$median_totnuf) #yes
acf(summary_medians$median_totpuf) #yes
acf(summary_medians$median_chla, na.action = na.pass) #no
acf(summary_medians$fil_algae_sum, na.action = na.pass) #no
acf(summary_medians$dWL, na.action = na.pass) #yes

# To assess differences in water clarity pre vs. post removal, we
# used a generalized least squares (GLS) model with an autoregressive
# correlation structure. Here, we modeled median Secchi depth
# (median_secchi) as a function of (group) using the gls()
# function from the nlme package in R. To account for potential temporal
# autocorrelation in repeated annual measurements, we included a first-order
# autoregressive correlation structure (corAR1) with respect to year (year4).
# This structure models residuals from consecutive years as being correlated,
# thereby improving model estimates when temporal dependence is present.

gls_secchi <- gls(median_secchi ~ removal, data = summary_medians, correlation = corAR1(form = ~ year4))
summary(gls_secchi)
residuals_gls <- resid(gls_secchi) # extract model residuals 
acf(residuals_gls, main = "ACF of GLS Residuals")  # If the AR(1) structure has effectively accounted for autocorrelation, 
# the ACF plot should show little to no significant autocorrelation

gls_tn <-  gls(median_totnuf ~ removal, data = summary_medians |> filter(!is.na(median_totnuf)), correlation = corAR1(form = ~ year4))
summary(gls_tn)

gls_tp <-  gls(median_totpuf ~ removal, data = summary_medians |> filter(!is.na(median_totpuf)), correlation = corAR1(form = ~ year4))
summary(gls_tp)

gls_chloro <- gls(median_chla ~ removal, data = summary_medians |> filter(!is.na(median_chla)))
summary(gls_chloro)

gls_fil <- gls(fil_algae_sum ~ removal, data = summary_medians |> filter(!is.na(fil_algae_sum)))
summary(gls_fil)

gls_dWL <- gls(dWL ~ removal, data = summary_medians |> filter(!is.na(dWL)), correlation = corAR1(form = ~ year4))
summary(gls_secchi)

