
# Mean FBUY
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
  geom_histogram(binwidth = 5) +
  ylab("Counts") +
  xlab('Mean FBUY') +
  scale_y_continuous(limits = c(0, 450)) +
  scale_x_continuous(limits = c(1850, 2015)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = 1865, y = 450, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == 1), 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = subset(mx_info, PANEL == 1),
            aes(label = round(x, 0), 
                x = 1875, y = 450-(450*0.056)), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = 1865, y = 450-(450*0.12), colour = "blue", size = 5) +
  geom_vline(aes(xintercept = mean_fbuy), data = mx_stats, 
             linetype = "dashed", color  = "blue") +
  geom_text(data = mx_stats, aes(label = round(mean_fbuy, 0), 
                x = 1875, y = 450-(450*0.176)), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = 1865, y = 450-(450*0.28), colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = pct_95th), data = mx_stats, 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = mx_stats, aes(label = round(pct_95th, 0), 
             x = 1875, y = 450-(450*0.336)), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")
ggsave("results/fbuy/mean_fbuy_histogram.pdf", p, width = 5, height = 4, dpi=600, scale = 3, units = "cm") #saves g


# Min FBUY
p_template <- as.data.frame(extraction_df) %>%
  filter(fbuy_min != 0) %>%
  ggplot(aes(x = fbuy_min)) +
  geom_histogram(binwidth = 5)

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
  filter(fbuy_min != 0) %>%
  group_by() %>%
  summarise(mean_fbuy = mean(fbuy_min),
            median_fbuy = median(fbuy_min),
            pct_95th = quantile(fbuy_min, probs = 0.95)) %>%
  ungroup()

p <- as.data.frame(extraction_df) %>%
  filter(fbuy_min != 0) %>%
  ggplot(aes(x = fbuy_min)) +
  geom_histogram(binwidth = 5) +
  ylab("Counts") +
  xlab('Mean FBUY') +
  scale_y_continuous(limits = c(0, 300)) +
  scale_x_continuous(limits = c(1850, 2015)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = 1865, y = 300, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == 1), 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = subset(mx_info, PANEL == 1),
            aes(label = round(x, 0), 
                x = 1875, y = 300-(300*0.056)), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = 1865, y = 300-(300*0.12), colour = "blue", size = 5) +
  geom_vline(aes(xintercept = mean_fbuy), data = mx_stats, 
             linetype = "dashed", color  = "blue") +
  geom_text(data = mx_stats, aes(label = round(mean_fbuy, 0), 
                                 x = 1875, y = 300-(300*0.176)), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = 1865, y = 300-(300*0.28), colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = pct_95th), data = mx_stats, 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = mx_stats, aes(label = round(pct_95th, 0), 
                                 x = 1875, y = 300-(300*0.336)), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")
ggsave("results/fbuy/min_fbuy_histogram.pdf", p, width = 5, height = 4, dpi=600, scale = 3, units = "cm") #saves g

