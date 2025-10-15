
library(patchwork)
library(scales)

################### Timeseries ###################
theme_timeseries <- function() {
  list(
    scale_x_date(
      limits = as.Date(c("1995-01-01", "2024-12-31")),
      breaks = seq(as.Date("1995-01-01"), as.Date("2024-01-01"), by = "5 years"),
      labels = date_format("%Y")
    ),
    xlab(""),
    scale_fill_manual(values = c("white", "black")),
    theme_bw(base_size = 9),
    theme(legend.position = "none", 
          axis.title.x = element_blank())
  )
}

secchi_timeseries = ggplot() +
  geom_point(data = secchi, aes(x = sampledate, y = secnview, fill = removal), size = 1.1, shape = 21, alpha = 0.5) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = mean_secchi), linewidth = 1) +
  ylab(expression(paste("Secchi (m)"))) +
  theme_timeseries() +
  geom_vline(aes(xintercept = as.Date('2008-01-15')), linewidth = 0.3, linetype = 2)

tn_timeseries <- ggplot() +
  geom_point(data = tn,aes(x = sampledate, y = totnuf, fill = removal), size = 1.1, shape = 21, alpha = 0.5) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = mean_totnuf), linewidth = 1) +
  ylab(bquote(atop("Total nitrogen", "(" * mu * "g " * L^{-1} * ")"))) +
  theme_timeseries() +
  geom_vline(aes(xintercept = as.Date('2008-06-18')), linewidth = 0.3, linetype = 2)

tp_timeseries <- ggplot() +
  geom_point(data = tp,aes(x = sampledate, y = totpuf, fill = removal), size = 1.1, shape = 21, alpha = 0.5) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = mean_totpuf), linewidth = 1) +
  ylab(bquote(atop("Total phosphorus", "(" * mu * "g " * L^{-1} * ")"))) +
  theme_timeseries() +
  geom_vline(aes(xintercept = as.Date('2008-07-02')), linewidth = 0.3, linetype = 2)

chloro_timeseries <- ggplot() +
  geom_point(data = chloro_all, aes(x = sampledate, y = chl_use, fill = removal),size = 1.1, shape = 21, alpha = 0.5) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = mean_chla), linewidth = 1) +
  ylab(bquote(atop("Chlorophyll a", "(" * mu * "g " * L^{-1} * ")"))) +
  theme_timeseries() +
  geom_vline(aes(xintercept = as.Date('2020-01-13')), linewidth = 0.3, linetype = 2)


ls7_timeseries <- ggplot() +
  geom_point(data = ls7, aes(x = sampledate, y = redblue, fill = removal), size = 1.1, shape = 21, alpha = 0.5) +
  geom_line(data = summary_means, aes(x = as.Date(paste0(year4, "-07-01")), y = redblue), linewidth = 1) +
  ylab('RBindex') +
  theme_timeseries()  +
  geom_vline(aes(xintercept = as.Date('2007-12-16')), linewidth = 0.3, linetype = 2)


macro_timeseries <- ggplot(macrophyte_timeseries) +
  geom_point(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281, fill = removal), size = 1.3, shape = 21) +
  geom_line(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281)) +
  ylab("Max colonization \ndepth (m)") +
  scale_fill_manual(values = c( "white", "black")) +
  xlab("") +
  xlim(c(1995, 2025)) +
  theme_bw(base_size = 9) + 
  theme(legend.position = "none")  +
  geom_vline(aes(xintercept = 2010.5), linewidth = 0.3, linetype = 2)


fil_timeseries <- ggplot(fil_algae_timeseries, aes(x = year4, y = fil_algae_sum, fill = removal)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
  # geom_vline(xintercept = 2008, linetype = "dashed") +
  xlab("") +
  scale_x_continuous(limits = c(1995,2025), breaks = seq(1995, 2025, 5)) +
  ylab("Fil. algae (wet mass \nper rake throw)") +
  scale_fill_manual(values = c( "white", "grey30")) +
  theme_bw(base_size = 9) + 
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = 2008.5), linewidth = 0.3, linetype = 2)

plant_timeseries <- ggplot(fil_algae_timeseries, aes(x = year4, y = plant_wt_sum, fill = removal)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
  # geom_vline(xintercept = 2008, linetype = "dashed") +
  xlab("") +
  scale_x_continuous(limits = c(1995,2025), breaks = seq(1995, 2025, 5)) +
  ylab("Macrophyte (wet mass \nper rake throw)") +
  scale_fill_manual(values = c( "white", "grey30")) +
  theme_bw(base_size = 9) + 
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = 2008.5), linewidth = 0.3, linetype = 2)



################### Boxplots ###################
theme_boxplot <- function() {
  list(
    scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x) >= 2008))),
    theme_bw(base_size = 9),
    scale_fill_manual(values = c( "white", "black")),
    theme(legend.position = "none",
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.line = element_line(colour = "black")))
}

secchi_boxplot <- ggplot(summary_means, aes(x = removal, y = mean_secchi)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=1, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()

tn_boxplot <- ggplot(summary_means, aes(x = removal, y = mean_totnuf)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=2, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()

tp_boxplot <- ggplot(summary_means, aes(x = removal, y = mean_totpuf)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=2, y=Inf, vjust = 1, label= "p < 0.01", size = 2) +
  theme_boxplot()

chloro_boxplot <- ggplot(summary_means, aes(x = removal, y = mean_chla)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=2, y=Inf, vjust = 1, label= "p = NS", size = 2) +
  theme_boxplot()

ls7_boxplot <- ggplot(summary_means, aes(x = removal, y = redblue)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=2, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()

fil_box<- ggplot(fil_algae_timeseries, aes(x = removal, y = fil_algae_sum)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5) +
  annotate("text", x=1, y=Inf, vjust = 2, label= "p < 0.01", size = 2) +
  theme_boxplot()



# 
# zoops_biomass_group<- zoop_biomass%>%
#   mutate(species_name = ifelse(species_name %in% "aglaodiaptomus clavipes", "diaptomus spp", species_name))%>%
#   mutate(species = ifelse(species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus", "chydorus"),
#                           "Cyclopoid",
#                           ifelse(species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes", "diacyclops thomasi", "diaptomid"),
#                                  "Calenoid",
#                                  ifelse(species_name %in% c("daphnia pulicaria", "daphnia retrocurva", "daphnia mendotae", "daphnia parvula", "diaphanosoma birgei", "daphnia"),
#                                         "Daphnia",
#                                         ifelse(species_name %in% c("sinobosmina fryei", "copepod nauplii", "copepodites"),
#                                                "Small Cladocera",
#                                                "Other")))))
# 
# zoops_density_group<- zoops_biomass_group%>%
#   group_by(sample_date, species_name)%>%
#   summarize(biomass = mean(mg_m3), .groups = 'drop')%>%
#   ungroup()%>%
#   mutate(species = ifelse(species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus",  "diacyclops thomasi","tropocyclops"),
#                           "Cyclopoid",
#                           ifelse(species_name %in% "copepod nauplii", 
#                                  "Nauplii",
#                                  ifelse(species_name %in% c("copepodites"), 
#                                         "Copepoda",
#                                         ifelse(species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes",  "diaptomid"),
#                                                "Calanoid",
#                                                ifelse(species_name %in% c("daphnia pulicaria",  "daphnia retrocurva", "daphnia mendotae", "daphnia parvula",  "daphnia"),
#                                                       "Daphnia",
#                                                       ifelse(species_name %in% c("sinobosmina fryei",  "chydorus", "diaphanosoma birgei", "ceriodaphnia dubia"),
#                                                              "Small Cladocera",
#                                                              "Other")))))))%>%
#   group_by(sample_date, species)%>%
#   summarize(biomass = sum(biomass))
# 
# zoops_density_group$species <- factor(zoops_density_group$species, levels = c("Calanoid",  "Cyclopoid",  "Nauplii", "Copepoda", "Daphnia", "Small Cladocera"))
# 
# zoop_timeseries_plot<- ggplot(zoops_density_group, aes(x = sample_date, y = biomass, fill = species)) +
#   geom_area(position = 'stack') +
#   ylab(bquote(atop("Zooplankton biomass", "(" * mu * "g " * L^{-1} * ")"))) +
#   xlab("") +
#   scale_fill_manual(values = c( "#762a83","#af8dc3",  "#e7d4e8", "#f7f7f4", "#008837", "#a6dba0")) +
#   guides(fill = guide_legend(
#     label = c("Calanoid", "Cyclopoid", "Nauplii", "Copepoda", 
#               expression(paste("Large Cladocera (", italic("Daphnia"), ")")), 
#               "Small Cladocera"))) +
#   scale_x_date(
#     limits = as.Date(c("1995-01-01", "2023-12-31")),
#     breaks = seq(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "5 years"),
#     labels = date_format("%Y")
#   ) +
#   theme_bw(base_size = 14) +
#   theme(legend.title= element_blank())

# Define areas: rows 1–7 for timeseries, column 1; rows 1–7 for boxplots, column 2
design <- c(
  area(1, 1),  # secchi_timeseries
  area(2, 1),  # tn_timeseries
  area(3, 1),  # tp_timeseries
  area(4, 1),  # chloro_timeseries
  area(5, 1),  # dWL_timeseries
  area(6, 1),  # macro_timeseries
  area(7, 1),  # fil_timeseries
  
  area(1, 2),  # secchi_boxplot
  area(2, 2),  # tn_boxplot
  area(3, 2),  # tp_boxplot
  area(4, 2),  # chloro_boxplot
  area(5, 2),  # dWL_boxplot
  # area(6, 2),  # blank_plot
  area(7, 2)   # fil_box
)

# Combine using design layout
wrap_plots(
  secchi_timeseries, tn_timeseries, tp_timeseries, chloro_timeseries,
  ls7_timeseries, macro_timeseries, fil_timeseries,
  secchi_boxplot, tn_boxplot, tp_boxplot, chloro_boxplot,
  ls7_boxplot, #blank_plot, 
  fil_box,
  design = design) +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8))

ggsave("figures/Figure1.png", width = 6.5, height = 8, units = 'in', dpi = 500)

