library(tidyverse)

tp_mean<- tp%>%
  group_by(year4)|>
  summarize(mean_totpuf = mean(totpuf))
chloro_mean<- chloro_all%>%
  group_by(year4)|>
  summarize(mean_chl = mean(chl_use))

chloro_tp<- tp_mean%>%
  left_join(chloro_mean, by = "year4")%>%
  left_join(fil_algae_timeseries, by = "year4")%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", "≥2008"))

#Linear regression
chloro_tp_post <- lm(mean_totpuf ~ mean_chl, data = filter(chloro_tp, year4> 2007 ))
summary(chloro_tp_post)
chloro_tp_pre <- lm(mean_totpuf ~ mean_chl, data = filter(chloro_tp, year4 < 2008 ))
summary(chloro_tp_pre)
  
ggplot(chloro_tp) +
  geom_smooth(aes(y = mean_chl, x = mean_totpuf, group = removal),method = "lm", color = "black")+
  geom_point(aes(y = mean_chl, x = mean_totpuf, fill = removal),shape = 21, size = 3)+
  scale_fill_manual(values = c( "white", "black"))+
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab(expression(paste("Total Phosphorus", " (µg ", L^-1,")")))+
  theme_bw()
ggsave("figures/Figure4.png", width = 6, height = 4, units = 'in')
