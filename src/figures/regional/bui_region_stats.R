# Plot BUI over time EAST vs WEST
prep_for_plot <- as.data.frame(extraction_df) %>%
  group_by(region, fire_bidecadal) %>%
  mutate(bui_ha = BUI*0.0001000000884,
         log_bui_ha = log(bui_ha)) %>%
  summarise(burn_area_ha = sum(mtbs_acres*0.404686),
            sum_bui = sum(bui_ha),
            mean_bui = mean(bui_ha),
            pct_95th = quantile(bui_ha, probs = 0.95),
            logmean_bui = log(mean_bui),
            logpct_95th = log(pct_95th)) %>%
  ungroup() %>%
  mutate(BUI_firearea_prop = sum_bui/burn_area_ha*100)

p1 <- prep_for_plot %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = fire_bidecadal, y = sum_bui)) +
  geom_bar(stat = 'identity') +
  ylab("Built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~region, ncol = 3)
ggsave("results/region/bui/bui_area_region.pdf", p1, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p2 <- prep_for_plot %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = fire_bidecadal, y = burn_area_ha*0.0001)) +
  geom_bar(stat = 'identity', fill = 'red') +
  ylab("Burned area (0.0001*ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~region, ncol = 3)
ggsave("results/region/bui/burn_area_region.pdf", p2, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p3 <- prep_for_plot %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  theme_pub()  +
  facet_wrap(~region, ncol = 3)
ggsave("results/region/bui/prop_region.pdf", p3, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p4 <- prep_for_plot %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = fire_bidecadal, y = mean_bui)) +
  geom_bar(stat = 'identity') +
  ylab("Mean built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~region, ncol = 3)
ggsave("results/region/bui/meanbui_area_region.pdf", p1, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

g <- arrangeGrob(p1, p2, p3, ncol = 1)
ggsave("results/region/bui/bui_firearea_prop_region.pdf", g, width = 8, height = 9, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))

