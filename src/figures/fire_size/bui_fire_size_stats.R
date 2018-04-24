
p_df <- as.data.frame(extraction_df) %>%
  mutate(bu_fire = ifelse(num_bu_pixels == 0, 0, 1)) %>%
  group_by(fire_bidecadal, fire_size) %>%
  summarise(n_fires = n(),
            num_bu_burned = sum(bu_fire),
            burned_area = sum(mtbs_acres*0.404686),
            bu_area = sum(bu_area),
            bui_ha = sum(BUI*0.0001000000884)) %>%
  mutate(pct_bu_in_burn = (num_bu_burned/n_fires)*100,
         pct_bu_area_burned = (bu_area/burned_area)*100,
         BUI_firearea_prop = bui_ha/burned_area*100) 

p1 <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = bui_ha)) +
  geom_bar(stat = 'identity') +
  ylab("Built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~fire_size, ncol = 4)
ggsave("results/fire_size/bui/bui_area_firesize.pdf", p1, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p2 <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = burned_area*0.0001)) +
  geom_bar(stat = 'identity', fill = 'red') +
  ylab("Burned area (0.0001*ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~fire_size, ncol = 4)
ggsave("results/fire_size/bui/burn_area_firesize.pdf", p2, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p3 <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  theme_pub()  +
  facet_wrap(~fire_size, ncol = 4)
ggsave("results/fire_size/bui/prop_firesize.pdf", p3, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

g <- arrangeGrob(p1, p2, p3, ncol = 1)
ggsave("results/fire_size/bui/bui_firearea_prop_firesize.pdf", g, width = 8, height = 9, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))
