
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


p_template <- as.data.frame(extraction_df) %>%
  filter(fbuy_mean != 0) %>%
  ggplot(aes(x = fbuy_mean)) +
  geom_histogram(binwidth = 1)

mx_cnt <- ggplot_build(p_template)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(y, x, count, PANEL) %>%
  group_by(PANEL) %>%
  summarize(max_count = max(count)) %>%
  ungroup()

mx_info <- ggplot_build(p_template)$data[[1]] %>%
  tbl_df %>%
  left_join(., mx_cnt, by = 'PANEL') %>%
  filter(y == max_count)

mx_stats <- as.data.frame(extraction_df) %>%
  filter(fbuy_mean != 0) %>%
  group_by() %>%
  summarise(mean_fbuy = mean(fbuy_mean),
            median_fbuy = median(fbuy_mean),
            pct_95th = quantile(fbuy_mean, probs = 0.95)) %>%
  ungroup()

p <- as.data.frame(extraction_df) %>%
  filter(fbuy_mean != 0) %>%
  ggplot(aes(x = fbuy_mean)) +
  geom_histogram(binwidth = 1) +
  ylab("Counts") +
  xlab('Mean FBUY') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(1850, 2015)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = 1865, y = 225, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == 1), 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = subset(mx_info, PANEL == 1),
            aes(label = round(x, 0), 
                x = 1875, y = 210), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = 1865, y = 195, colour = "blue", size = 5) +
  geom_vline(aes(xintercept = mean_fbuy), data = mx_stats, 
             linetype = "dashed", color  = "blue") +
  geom_text(data = mx_stats, aes(label = round(mean_fbuy, 0), 
                x = 1875, y = 175), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = 1865, y = 160, colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = pct_95th), data = mx_stats, 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = mx_stats, aes(label = round(pct_95th, 0), 
             x = 1875, y = 145), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")
ggsave("results/fbuy/fbuy_histogram.pdf", p, width = 5, height = 4, dpi=600, scale = 3, units = "cm") #saves g


