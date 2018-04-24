
p_df <- as.data.frame(extraction_df) %>%
  mutate(bu_fire = ifelse(num_bu_pixels == 0, 0, 1)) %>%
  group_by(region, fire_bidecadal, fire_size) %>%
  summarise(n_fires = n(),
            num_bu_burned = sum(bu_fire),
            burned_area = sum(mtbs_acres*0.404686),
            bu_area = sum(bu_area),
            bui_ha = sum(BUI*0.0001000000884)) %>%
  mutate(pct_bu_in_burn = (num_bu_burned/n_fires)*100,
         pct_bu_area_burned = (bu_area/burned_area)*100,
         BUI_firearea_prop = bui_ha/burned_area*100) 

p1 <- p_df %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = bui_ha, fill = region)) +
  geom_bar(stat = 'identity') +
  ylab("Built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(region~fire_size, ncol = 4) +
  theme(legend.position = 'none')
ggsave("results/fire_size/bui/bui_region_firesize.pdf", p1, width = 8, height = 6, dpi=600, scale = 3, units = "cm") #saves g

p2 <- p_df %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = burned_area*0.0001, fill = region)) +
  geom_bar(stat = 'identity') +
  ylab("Burned area (0.0001*ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(region~fire_size, ncol = 4) +
  theme(legend.position = 'none')
ggsave("results/fire_size/bui/burn_region_firesize.pdf", p2, width = 8, height = 6, dpi=600, scale = 3, units = "cm") #saves g

p3 <- p_df %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop, fill = region)) +
  geom_bar(stat = 'identity') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  theme_pub()  +
  facet_wrap(region~fire_size, ncol = 4) +
  theme(legend.position = 'none')
ggsave("results/fire_size/bui/prop_region_firesize.pdf", p3, width = 8, height = 6, dpi=600, scale = 3, units = "cm") #saves g

system(paste0("aws s3 cp results ", s3_results, ' --recursive'))
