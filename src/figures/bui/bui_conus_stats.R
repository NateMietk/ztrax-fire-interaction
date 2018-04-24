# Plot BUI over time
prep_for_plot <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  group_by(fire_bidecadal) %>%
  summarise(burn_area_ha = sum(mtbs_acres*0.404686),
            bui_ha = sum(bui_ha),
            mean_bui = mean(bui_ha)) %>%
  mutate(BUI_firearea_prop = bui_ha/burn_area_ha*100)


p1 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = bui_ha)) +
  geom_bar(stat = 'identity') +
  ylab("Built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  ggtitle('Total built-up intensity (ha)') +
  theme_pub()
ggsave("results/bui/bui_area.pdf", p1, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g

p2 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = burn_area_ha)) +
  geom_bar(stat = 'identity', fill = 'red') +
  ylab("Burned area (ha)") +
  xlab('Bidecadal fire year') +
  ggtitle('Burned area (ha)') +
  theme_pub()
ggsave("results/bui/burn_area.pdf", p2, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g

p3 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  ggtitle('Proportion of BUI to \nwildfire burned area') +
  theme_pub()
ggsave("results/bui/bui_prop.pdf", p3, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g

g <- arrangeGrob(p1, p2, p3, nrow = 1)
ggsave("results/bui/bui_firearea_prop.pdf", g, width = 9, height = 6, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))


# Plot BUI over time
df1 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  group_by() %>%
  summarise(tot_burnarea = sum(mtbs_acres*0.404686))
            
prep_for_plot <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  group_by(fire_bidecadal) %>%
  summarise(burn_area_ha = sum(mtbs_acres*0.404686),
            bui_ha = sum(bui_ha),
            mean_bui = mean(bui_ha)) %>%
  mutate(BUI_firearea_prop = bui_ha/df1$tot_burnarea*100)

p3 <- prep_for_plot %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  ggtitle('Proportion of BUI to \ntotal wildfire burned area') +
  theme_pub()
ggsave("results/bui/bui_total_fire_prop.pdf", p3, width = 6, height = 4, dpi=600, scale = 3, units = "cm") #saves g
