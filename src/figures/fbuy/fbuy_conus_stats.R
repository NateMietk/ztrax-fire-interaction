
fbuy_plot_stats <- function(df, y, title, xlab, ylab) {
  
  p <- df %>%
    ggplot() +
    geom_bar(aes(x = fire_bidecadal, y = y), 
             stat = 'identity', position = 'dodge') +
    theme_pub() +
    ggtitle(title) +
    ylab(xlab) +
    xlab(ylab) +
    theme(legend.position = "none")
  p
}

p_df <- as.data.frame(extraction_df) %>%
  mutate(bu_fire = ifelse(num_bu_pixels == 0, 0, 1)) %>%
  group_by(fire_bidecadal, region) %>%
  summarise(n_fires = n(),
            num_bu_burned = sum(bu_fire),
            burned_area = sum(mtbs_acres*0.404686),
            bu_area = sum(bu_area)) %>%
  mutate(pct_bu_in_burn = (num_bu_burned/n_fires)*100,
         pct_bu_area_burned = (bu_area/burned_area)*100) 


p_bu_num <- fbuy_plot_stats(p_df, y = p_df$pct_bu_in_burn, 
                            title = 'Proportion of built-up area \nto wildfire area',
                            xlab = '% built-up to wildfire area',
                            ylab = 'Years')

p_bu_pct <- fbuy_plot_stats(p_df, y = p_df$pct_bu_area_burned, 
                            title = 'Proportion of built-up area \nto wildfire area',
                            xlab = '% built-up to wildfire area',
                            ylab = 'Years')


p_bu_area <- fbuy_plot_stats(p_df, y = p_df$bu_area, 
                             title = 'Built-up area witin wildfire (ha)',
                             xlab = 'BU area (ha)',
                             ylab = 'Years')

g <- arrangeGrob(p_bu_num, p_bu_pct, p_bu_area, nrow = 1)
ggsave("results/fbuy/fbuy_stats.pdf", g, width = 10, height = 5, dpi=600, scale = 3, units = "cm") #saves g
