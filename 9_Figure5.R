library(tidyverse)
library(lavaan) # for SEM
library(semPlot)
library(cowplot)

# Renaming just so the final plot is legible 
df = summary_medians |>  
  left_join(zoops_year |> filter(group == 'Daphnia') |> rename(year4 = year)) |> 
  rename(filA = fil_algae_sum, precip = arb.precip,
         chl = median_chla, secchi = median_secchi, TP = median_totpuf, 
         TN = median_totnuf, daphnia = density) |> 
  filter(removal == '>=2008') |> # just analyze post group
  select(secchi, TN, TP, chl, filA, precip, daphnia) |> 
  mutate(
    TN_mol = TN / 14.01,
    TP_mol = TP / 30.97,
    TN_TP_molar = TN_mol / TP_mol
  )|>
  mutate(across(everything(), ~ scale(.))) # standardize all columns

########## Model focused on nitrogen ##########
# Define the structural model
model <- '
  # Nutrients influenced by precipitation
  TN ~ precip
  secchi ~ precip

  # TP influencing Chloro and Sum Filamentous Algae
  # chl ~ TP
  # filA ~ TP

  chl ~ TN
  filA ~ TN
  
  # Secchi influence
  filA ~ secchi
  chl ~ secchi
  daphnia ~ secchi
  
  # Fila and zoops 
  filA ~ daphnia
  chl ~ daphnia
'

# Fit the model
fit <- sem(model, data = df)

# Summary of the model
summary(fit, standardized = TRUE)

# Plot the SEM model
semPaths(fit, 
         what = "std",      # standardized estimates
         whatLabels = 'est',
         layout = "tree2",   # tree layout
         edge.label.cex = 1, # size of the edge labels
         sizeMan = 8,       # size of manifest variables
         sizeLat = 10,      # size of latent variables
         edge.color = "black",
         curveAdjacent = F,
         style = "lisrel",
         nCharNodes = 7,
         residuals = FALSE,  # show residuals
         intercepts = FALSE # hide intercepts
)

########## Model focused on phosphorus ##########

model2 <- '
  # Nutrients influenced by precipitation
  TP ~ precip
  secchi ~ precip

  # TP influencing Chloro and Sum Filamentous Algae
  chl ~ TP
  filA ~ TP
  
  # Secchi influence
  filA ~ secchi
  chl ~ secchi
  daphnia ~ secchi
  
  # Fila and zoops 
  filA ~ daphnia
  chl ~ daphnia
'

# Fit the model
fit2 <- sem(model2, data = df)

# Summary of the model
# summary(fit2, standardized = TRUE)
sem_model <- semPlot::semPlotModel(fit2)

# Plot the SEM model
s1 = semPaths(fit2, 
              what = "std",      # standardized estimates
              whatLabels = 'est',
              layout = "tree2",   # tree layout
              edge.label.cex = 1, # size of the edge labels
              sizeMan = 8,       # size of manifest variables
              sizeLat = 10,      # size of latent variables
              edge.color = "black",
              curveAdjacent = F,
              style = "lisrel",
              nCharNodes = 7,
              residuals = FALSE,  # show residuals
              intercepts = FALSE # hide intercepts
)

png("figures/FigureSEM.png", width = 3, height = 2.5, units = 'in', res = 500)
semPaths(fit2, what = "std", whatLabels = "est", layout = "tree2",
         edge.label.cex = 2, 
         sizeMan = 12, sizeLat = 10, edge.color = "black",
         curveAdjacent = FALSE, style = "lisrel", nCharNodes = 7,
         residuals = FALSE, intercepts = FALSE, 
         sizeInt = 5,
         margin = 0.1, 
         label.cex = 1.2,
         layoutScale = 1.3)
dev.off()

# Zooplankton relationship with filamentous algae
summary_means %>%
  left_join(zoops_year |> rename(year4 = year)) |> 
  filter(year4 >= 2008) %>%
  filter(group %in% c("Daphnia", "Copepoda")) %>%
  group_by(group) %>%
  do(tidy(lm(density ~ fil_algae_sum, data = .)))

# Code to Extract R2
summary_means %>%
  left_join(zoops_year %>% rename(year4 = year)) %>%
  filter(year4 >= 2008) %>%
  filter(group %in% c("Daphnia", "Copepoda")) %>%
  group_by(group) %>%
  do(glance(lm(density ~ fil_algae_sum, data = .))) %>%
  select(group, r.squared, adj.r.squared, p.value)

z1 = summary_means |> left_join(zoops_year |> rename(year4 = year)) |> 
  filter(year4 >= 2008) |> 
  filter(group %in% c('Daphnia','Copepoda')) |> 
  ggplot() +
  geom_smooth(aes(x = fil_algae_sum, y = density, color = group, fill = group), method = "lm") +
  geom_point(aes(x = fil_algae_sum, y = density, fill = group), size = 1.2, shape = 21) +
  scale_fill_manual(values = c('grey20','grey80')) +
  scale_color_manual(values = c('grey20','grey80')) +
  # facet_wrap(~group) +
  ylab('Median summer zooplankton density (#/L)') +
  # ylab(expression(paste("Mean summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  xlab("Fil. algae (wet mass per rake throw)")+
  theme_bw(base_size = 9) +
  theme(legend.position = 'inside', legend.position.inside = c(0.25,0.8), legend.title = element_blank())

# Load SEM plot and combine 
sem_image <- ggdraw() + draw_image("figures/FigureSEM.png")
z1 + sem_image + 
  plot_layout(widths = c(1,1.5)) +  # side-by-side
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8))
ggsave("figures/Figure5.png", width = 6.5, height = 3, units = 'in', dpi = 500)

