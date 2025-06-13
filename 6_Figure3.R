library(tidyverse)
library(patchwork)

summary_medians<- read_csv("data/summary_medians.csv") |>
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

precip_secchi<-ggplot(summary_medians) +
  geom_smooth(aes(x = arb.precip, y = median_secchi, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = median_secchi, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Secchi (m)")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_tn<-ggplot(summary_medians) +
  geom_smooth(aes(x = arb.precip, y = median_totnuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = median_totnuf, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total nitrogen", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_tp<- ggplot(summary_medians) +
  geom_smooth(aes(x = arb.precip, y = median_totpuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = median_totpuf, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total phosphorus", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_chloro<-ggplot(summary_medians) +
  geom_smooth(aes(x = arb.precip, y = median_chla, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = median_chla, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

(precip_secchi + precip_tp) / (precip_tn + precip_chloro) + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &theme(legend.position='bottom') 
ggsave("figures/Figure3.png", width = 8, height = 7, units = 'in')
