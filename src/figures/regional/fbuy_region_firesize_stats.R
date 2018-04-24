
p_df1 <- as.data.frame(extraction_df) %>%
  mutate(bu_fire = ifelse(num_bu_pixels == 0, 0, 1)) %>%
  group_by(fire_size, region) %>%
  summarise(ntotal_fires = n())
            
p_df <- as.data.frame(extraction_df) %>%
  mutate(bu_fire = ifelse(num_bu_pixels == 0, 0, 1)) %>%
  group_by(fire_size, fire_bidecadal, region) %>%
  summarise(n_fires = n(),
            num_bu_burned = sum(bu_fire),
            burned_area = sum(mtbs_acres*0.404686),
            bu_area = sum(bu_area)) %>%
  left_join(., p_df1, by = c('fire_size', 'region')) %>%
  mutate(pct_bu_in_burn = (num_bu_burned/n_fires)*100,
         pct_bu_in_total_fires = (num_bu_burned/ntotal_fires)*100,
         pct_bu_area_burned = (bu_area/burned_area)*100)

p1 <- p_df %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = pct_bu_in_burn, fill = region)) +
  geom_bar(stat = 'identity') +
  ylab("% wildfires that burned in built-up areas") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(region~fire_size, ncol = 4) +
  theme(legend.position = 'none')
ggsave("results/fire_size/fbuy/fbuy_pct_region_firesize.pdf", p1, width = 8, height = 6, dpi=600, scale = 3, units = "cm") #saves g

p2 <- p_df %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = pct_bu_in_total_fires, fill = region)) +
  geom_bar(stat = 'identity') +
  ylab("% of total wildfires that burned in built-up areas") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(region~fire_size, ncol = 4) +
  theme(legend.position = 'none')
ggsave("results/fire_size/fbuy/fbuy_pctoftotal_region_firesize.pdf", p2, width = 8, height = 6, dpi=600, scale = 3, units = "cm") #saves g
