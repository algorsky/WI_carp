library(tidyverse)
library(patchwork)

# get model significance
get_sig_models <- function(data, x, y, group_var = removal) {
  x_var <- as_label(enquo(x))
  y_var <- as_label(enquo(y))
  group_var <- enquo(group_var)
  
  data %>%
    group_by(!!group_var) %>%
    do({
      model <- lm(reformulate(x_var, y_var), data = .)
      tidy_model <- tidy(model)
      slope_p <- tidy_model %>%
        filter(term == x_var) %>%
        pull(p.value)
      if (length(slope_p) == 0) slope_p <- NA
      mutate(., sig_line = slope_p < 0.05, p = slope_p)
    }) %>%
    ungroup()
}

get_sig_models(summary_means, x = arb.precip, y = mean_secchi)

precip_secchi <- ggplot(summary_means) +
  # Plot sig-only lines
  geom_smooth(
    data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_secchi), sig_line),
    aes(arb.precip, y = mean_secchi, group = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  # geom_smooth(aes(x = arb.precip, y = mean_secchi, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_secchi, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Secchi (m)")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 9)

precip_tn <- ggplot(summary_means) +
  geom_smooth(
    data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_totnuf), sig_line),
    aes(x = arb.precip, y = mean_totnuf, group = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  # geom_smooth(aes(x = arb.precip, y = mean_totnuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_totnuf, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total nitrogen", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 9)

precip_tp <- ggplot(summary_means) +
  geom_smooth(
    data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_totpuf), sig_line),
    aes(x = arb.precip, y = mean_totpuf, group = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  # geom_smooth(aes(x = arb.precip, y = mean_totpuf, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_totpuf, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total phosphorus", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 9)

precip_chloro <- ggplot(summary_means) +
  geom_smooth(
    data = filter(get_sig_models(summary_means, x = arb.precip, y = mean_chla), sig_line),
    aes(x = arb.precip, y = mean_chla, group = removal),
    method = "lm",
    color = "black", size = 0.4
  ) +
  # geom_smooth(aes(x = arb.precip, y = mean_chla, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = mean_chla, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 9)

precip_redblue <- ggplot(summary_means) +
  geom_smooth(aes(x = arb.precip, y = redblue, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = redblue, fill = removal), shape = 21, size = 2.5) +
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab('RBindex') +
  xlab("Precipitation (mm)") +
  theme_bw(base_size = 9)

(precip_secchi + precip_tp) / (precip_tn + precip_chloro) + 
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8)) 
ggsave("figures/Figure3.png", width = 6.5, height = 5, units = 'in', dpi = 500)
