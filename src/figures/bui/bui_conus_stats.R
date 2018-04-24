# Plot BUI over time
prep_for_plot <- as.data.frame(extraction_df) %>%
  group_by(fire_bidecadal) %>%
  summarise(burn_area_ha = sum(mtbs_acres*0.404686),
            bui_ha = sum(BUI*0.0001000000884)) %>%
  mutate(BUI_firearea_prop = bui_ha/burn_area_ha*100)

p1 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = bui_ha)) +
  geom_bar(stat = 'identity') +
  ylab("Built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub()
ggsave("results/bui/bui_area.pdf", p1, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g

p2 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = burn_area_ha)) +
  geom_bar(stat = 'identity', fill = 'red') +
  ylab("Burned area (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub()
ggsave("results/bui/burn_area.pdf", p2, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g

p3 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  theme_pub()
ggsave("results/bui/prop.pdf", p3, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g

g <- arrangeGrob(p1, p2, p3)
ggsave("results/bui/bui_firearea_prop.pdf", g, width = 6, height = 9, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))