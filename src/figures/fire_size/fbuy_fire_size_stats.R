
fbuy_plot_stats <- function(df, y, title, xlab, ylab) {
  
  p <- df %>%
    ggplot() +
    geom_bar(aes(x = fire_bidecadal, y = y), 
             stat = 'identity', position = 'dodge') +
    theme_pub() +
    ggtitle(title) +
    ylab(xlab) +
    xlab(ylab) +
    theme(legend.position = "none") +
    facet_wrap(~fire_size)
  p
}

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


p_bu_num <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  fbuy_plot_stats(., y = p_df$pct_bu_in_burn, 
                  title = 'Proportion of the number of \nwildfires that had built-up',
                  xlab = '% built-up to wildfire count',
                  ylab = 'Years')
ggsave("results/fire_size/fbuy/fbuy_pro_number_fire_size.pdf", p_bu_num, width = 5, height = 5, dpi=600, scale = 3, units = "cm") #saves 

p_bu_area <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("500 - 5000", "5000 - 25000", '25000 - 50000', '> 50000'))) %>%
  fbuy_plot_stats(.,y = p_df$bu_area, 
                  title = 'Built-up area within wildfire (ha)',
                  xlab = 'BU area (ha)',
                  ylab = 'Years')
ggsave("results/fire_size/fbuy/fbuy_bu_area_fire_size.pdf", p_bu_num, width = 5, height = 5, dpi=600, scale = 3, units = "cm") #saves 

