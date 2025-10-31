# Time series of maximum spring Secchi depth


ggplot(secchi.may, aes(x = year4, y = secnview)) +
  geom_line(linewidth = 0.2) +
  geom_point(aes(fill = removal), size = 1.1, shape = 21, alpha = 1) +
  ylab("Maximum May Secchi depth (m)") +
  theme_timeseries() +
  scale_fill_manual(values = c("white", "black")) +
  theme_bw(base_size = 9) +
  theme(legend.position = "none", 
      axis.title.x = element_blank())

ggsave('figures/FigureS2.png', width = 4, height = 2, dpi = 500)
