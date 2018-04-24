

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
  transform(fire_size = factor(fire_size, levels=c("< 1000", "1000 - 10000", '10000 - 50000', '> 50000'))) %>%
  fbuy_plot_stats(., y = p_df$pct_bu_in_burn, 
                  title = 'Proportion of built-up area \nto wildfire area',
                  xlab = '% built-up to wildfire area',
                  ylab = 'Years')

p_bu_pct <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("< 1000", "1000 - 10000", '10000 - 50000', '> 50000'))) %>%
  fbuy_plot_stats(.,y = p_df$pct_bu_area_burned, 
                  title = 'Proportion of built-up area \nto wildfire area',
                  xlab = '% built-up to wildfire area',
                  ylab = 'Years')


p_bu_area <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("< 1000", "1000 - 10000", '10000 - 50000', '> 50000'))) %>%
  fbuy_plot_stats(.,y = p_df$bu_area, 
                 title = 'Built-up area witin wildfire (ha)',
                 xlab = 'BU area (ha)',
                 ylab = 'Years')

g <- arrangeGrob(p_bu_num, p_bu_pct, p_bu_area, nrow = 1)
ggsave("results/fbuy/fbuy_stats_fire_size.pdf", g, width = 10, height = 5, dpi=600, scale = 3, units = "cm") #saves 


p1 <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("< 1000", "1000 - 10000", '10000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = bui_ha)) +
  geom_bar(stat = 'identity') +
  ylab("Built-up intentsity (ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~fire_size, ncol = 4)
ggsave("results/region/bui/bui_area_firesize.pdf", p1, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p2 <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("< 1000", "1000 - 10000", '10000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = burned_area*0.0001)) +
  geom_bar(stat = 'identity', fill = 'red') +
  ylab("Burned area (0.0001*ha)") +
  xlab('Bidecadal fire year') +
  theme_pub() +
  facet_wrap(~fire_size, ncol = 4)
ggsave("results/region/bui/burn_area_firesize.pdf", p2, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

p3 <- p_df %>%
  transform(fire_size = factor(fire_size, levels=c("< 1000", "1000 - 10000", '10000 - 50000', '> 50000'))) %>%
  ggplot(aes(x = fire_bidecadal, y = BUI_firearea_prop)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  ylab("Proportion of BUI to burned area (%)") +
  xlab('Bidecadal fire year') +
  theme_pub()  +
  facet_wrap(~fire_size, ncol = 4)
ggsave("results/region/bui/prop_firesize.pdf", p3, width = 8, height = 3, dpi=600, scale = 3, units = "cm") #saves g

g <- arrangeGrob(p1, p2, p3, ncol = 1)
ggsave("results/region/bui/bui_firearea_prop_firesize.pdf", g, width = 8, height = 9, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))


# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '< 1000', year = '1990', panel = '1', 
                      upper = 40, conus =FALSE)

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '< 1000', year = '1995', panel = '2', 
                      upper = 40, conus =FALSE)

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '< 1000',  year = '2000', panel = '3', 
                      upper = 40, conus =FALSE)

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '< 1000', year = '2005', panel = '4', 
                      upper = 40, conus =FALSE)

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '< 1000',  year = '2010', panel = '5', 
                      upper = 40, conus =FALSE)

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '< 1000',  year = '2015', panel = '6', 
                      upper = 40, conus =FALSE)

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/bui/bui_hist_per5yr_1000.pdf", g, width = 13, height = 4, dpi=600, scale = 4, units = "cm") #saves g



# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '1000 - 10000', year = '1990', panel = '1', 
                      upper = 200, conus =FALSE)

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '1000 - 10000', year = '1995', panel = '2', 
                      upper = 200, conus =FALSE)

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '1000 - 10000',  year = '2000', panel = '3', 
                      upper = 200, conus =FALSE)

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '1000 - 10000', year = '2005', panel = '4', 
                      upper = 200, conus =FALSE)

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '1000 - 10000',  year = '2010', panel = '5', 
                      upper = 200, conus =FALSE)

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '1000 - 10000',  year = '2015', panel = '6', 
                      upper = 200, conus =FALSE)

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/bui/bui_hist_per5yr_1000_10000.pdf", g, width = 13, height = 4, dpi=600, scale = 4, units = "cm") #saves g


# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '10000 - 50000', year = '1990', panel = '1', 
                      upper = 50, conus =FALSE)

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '10000 - 50000', year = '1995', panel = '2', 
                      upper = 50, conus =FALSE)

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '10000 - 50000',  year = '2000', panel = '3', 
                      upper = 50, conus =FALSE)

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '10000 - 50000', year = '2005', panel = '4', 
                      upper = 50, conus =FALSE)

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '10000 - 50000',  year = '2010', panel = '5', 
                      upper = 50, conus =FALSE)

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '10000 - 50000',  year = '2015', panel = '6', 
                      upper = 50, conus =FALSE)

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/bui/bui_hist_per5yr_10000_50000.pdf", g, width = 13, height = 4, dpi=600, scale = 4, units = "cm") #saves g

# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '> 50000', year = '1990', panel = '1', 
                      upper = 15, conus =FALSE)

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '> 50000', year = '1995', panel = '2', 
                      upper = 15, conus =FALSE)

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '> 50000',  year = '2000', panel = '3', 
                      upper = 15, conus =FALSE)

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '> 50000', year = '2005', panel = '4', 
                      upper = 15, conus =FALSE)

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '> 50000',  year = '2010', panel = '5', 
                      upper = 15, conus =FALSE)

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot_fire_size(., fire_sizes = '> 50000',  year = '2015', panel = '6', 
                      upper = 15, conus =FALSE)

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/bui/bui_hist_per5yr_50000.pdf", g, width = 13, height = 4, dpi=600, scale = 4, units = "cm") #saves g

