library(tidyverse)

ggplot(filter(summary_medians, year4 > 2007))+
  geom_smooth(aes(x = fil_algae_sum, y = zoop_summer_mgm3), method = "lm", color = "black")+
  geom_point(aes(x = fil_algae_sum, y = zoop_summer_mgm3), size = 3)+
  ylab(expression(paste("Mean summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  xlab("Fil. algae \n (wet mass per rake throw)")+
  theme_bw(base_size = 12)

ggsave("figures/Figure5.png", width = 6, height = 4, units = 'in')

zoop_fil_lm <- lm(zoop_summer_mgm3 ~ fil_algae_sum, data = filter(summary_medians, year4 > 2007 ))
summary(zoop_fil_lm)
