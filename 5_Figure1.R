library(tidyverse)
library(patchwork)
library(scales)

summary_medians<- read_csv("data/summary_medians.csv") |>
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

secchi_timeseries<- ggplot()+
  geom_point(data = secchi,aes(x = sampledate, y = secnview, fill = removal),size = 2.5,  shape = 21, alpha = 0.5)+
  geom_line(data = summary_medians, aes(x = as.Date(paste0(year4, "-07-01")), y = median_secchi), size = 1.5)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  # scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  ylab(expression(paste("Secchi \n(m)")))+
  scale_x_date(
    limits = as.Date(c("1995-01-01", "2023-12-31")),
    breaks = seq(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "5 years"),
    labels = date_format("%Y")
  )+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

secchi_boxplot<- ggplot(summary_medians, aes(x = removal, y = median_secchi))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=2, label= "p < 0.01")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))

tn_timeseries<-ggplot()+
  #geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_point(data = tn,aes(x = sampledate, y = totnuf, fill = group),size = 2.5,  shape = 21, alpha = 0.5)+
  geom_line(data = summary_medians, aes(x = as.Date(paste0(year4, "-07-01")), y = median_totnuf), size = 1.5)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  # scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  scale_x_date(
    limits = as.Date(c("1995-01-01", "2023-12-31")),
    breaks = seq(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "5 years"),
    labels = date_format("%Y")
  )+
  ylab(bquote(atop("Total nitrogen", "(" * mu * "g " * L^{-1} * ")")))+
  xlab("")+
  scale_fill_manual(values = c("black", "white"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))
tn_boxplot<-ggplot(summary_medians, aes(x = removal, y = median_totnuf))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=2, y=1600, label= "p < 0.01")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))

tp_timeseries<-ggplot()+
  #geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_point(data = tp,aes(x = sampledate, y = totpuf, fill = group),size = 2.5,  shape = 21, alpha = 0.5)+
  geom_line(data = summary_medians, aes(x = as.Date(paste0(year4, "-07-01")), y = median_totpuf), size = 1.5)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  # scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  scale_x_date(
    limits = as.Date(c("1995-01-01", "2023-12-31")),
    breaks = seq(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "5 years"),
    labels = date_format("%Y")
  )+
  ylab(bquote(atop("Total phosphorus", "(" * mu * "g " * L^{-1} * ")")))+
  xlab("")+
  scale_fill_manual(values = c("black", "white"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

tp_boxplot<-ggplot(summary_medians, aes(x = removal, y = median_totpuf))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=2, y=70, label= "p < 0.01")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))

chloro_timeseries<-ggplot()+
  #geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_point(data = chloro_all,aes(x = sampledate, y = chl_use, fill = removal),size = 2.5,  shape = 21, alpha = 0.5)+
  geom_line(data = summary_medians, aes(x = as.Date(paste0(year4, "-07-01")), y = median_chla), size = 1.5)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  # scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  ylab(bquote(atop("Chlorophyll a", "(" * mu * "g " * L^{-1} * ")")))+
  scale_x_date(
    limits = as.Date(c("1995-01-01", "2023-12-31")),
    breaks = seq(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "5 years"),
    labels = date_format("%Y")
  )+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))
chloro_boxplot<-ggplot(summary_medians, aes(x = removal, y = median_chla))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=35, label= "p < 0.05")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))

fil_timeseries<-ggplot(fil_algae_timeseries, aes(x = year4, y = fil_algae_sum, fill = removal))+
  geom_bar(stat = "identity", color = "black")+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab("Fil. algae \n (wet mass per rake throw)")+
  scale_fill_manual(values = c( "white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))

fil_box<-ggplot(fil_algae_timeseries, aes(x = removal, y = fil_algae_sum))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  annotate("text", x=1, y=9000, label= "p < 0.01")+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))

macro_timeseries<-ggplot(macrophyte_timeseries)+
  geom_point(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281, fill = removal), size = 4, shape = 21)+
  geom_line(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281))+
  ylab("Max colonization depth \n(m)")+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  xlim(c(1995, 2023))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none")

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
  ylab(bquote(atop("Zooplankton biomass", "(" * mu * "g " * L^{-1} * ")")))+
  xlab("")+
  scale_fill_manual(values = c( "#762a83","#af8dc3",  "#e7d4e8", "#f7f7f4", "#008837", "#a6dba0"))+
  guides(fill = guide_legend(
    label = c("Calanoid", "Cyclopoid", "Nauplii", "Copepoda", 
              expression(paste("Large Cladocera (", italic("Daphnia"), ")")), 
              "Small Cladocera")))+
  scale_x_date(
    limits = as.Date(c("1995-01-01", "2023-12-31")),
    breaks = seq(as.Date("1995-01-01"), as.Date("2023-01-01"), by = "5 years"),
    labels = date_format("%Y")
  )+
  theme_bw(base_size = 14)+
  theme(legend.title= element_blank())
blank_plot <- ggplot() + theme_void()  
layout <- (secchi_timeseries/tn_timeseries/tp_timeseries/chloro_timeseries/macro_timeseries/zoop_timeseries_plot/fil_timeseries) | (secchi_boxplot/tn_boxplot/tp_boxplot/chloro_boxplot/blank_plot/blank_plot/blank_plot/fil_box)

# Adjust the relative widths: first column (time series) twice as wide as second column (box plots)
layout + 
  plot_layout(widths = c(2, 1)) +  # First column twice as wide
  plot_annotation(tag_levels = 'a', tag_prefix = "(",tag_suffix = ")")
ggsave("figures/Figure1.png", width = 14, height = 16, units = 'in')

