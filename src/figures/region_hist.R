
p5 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  theme_pub() +
  facet_wrap(~region, ncol = 3, scales = 'free')

mx_cnt <- ggplot_build(p5)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(y, x, count, PANEL) %>%
  group_by(PANEL) %>%
  summarize(max_count = max(count)) %>%
  ungroup()
mx_info <- ggplot_build(p5)$data[[1]] %>%
  tbl_df %>%
  left_join(., mx_cnt, by = 'PANEL') %>%
  filter(y == max_count)

mx_stats <- as.data.frame(extraction_df) %>%
  group_by(region) %>%
  mutate(bui_ha = BUI*0.0001000000884,
         log_bui_ha = log(bui_ha)) %>%
  summarise(mean_bui = mean(bui_ha),
            pct_95th = quantile(bui_ha, probs = 0.95),
            logmean_bui = log(mean_bui),
            logpct_95th = log(pct_95th)) %>%
  ungroup()

# check to see where the min. diffs fall in plot
firefreq_cent <- as.data.frame(extraction_df) %>%
  filter(region ==  "Central") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  #transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  ggtitle("Central") +
  scale_y_continuous(limits = c(0, 210)) +
  scale_x_continuous(limits = c(-8, 8)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = -7, y = 200, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == '2'), 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = subset(mx_info, PANEL == '2'),
            aes(label=paste(formatC(round(exp(x)*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 185), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = -7, y = 160, colour = "blue", size = 5) +
  geom_vline(aes(xintercept = logmean_bui), data = subset(mx_stats, region == 'Central'), 
             linetype = "dashed", color  = "blue") +
  geom_text(data = subset(mx_stats, region == 'Central'),
            aes(label = paste(formatC(round(mean_bui*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 145), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = -7, y = 125, colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = logpct_95th), data = subset(mx_stats, region == 'Central'), 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = subset(mx_stats, region == 'Central'),
            aes(label = paste(formatC(round(pct_95th*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 110), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_east <- as.data.frame(extraction_df) %>%
  filter(region ==  "East") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  #transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  ggtitle("East") +
  scale_y_continuous(limits = c(0, 300)) +
  scale_x_continuous(limits = c(-8, 8)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = -7, y = 300, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == '1'), 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = subset(mx_info, PANEL == '1'),
            aes(label=paste(formatC(round(exp(x)*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 285), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = -7, y = 260, colour = "blue", size = 5) +
  geom_vline(aes(xintercept = logmean_bui), data = subset(mx_stats, region == 'East'), 
             linetype = "dashed", color  = "blue") +
  geom_text(data = subset(mx_stats, region == 'East'),
            aes(label = paste(formatC(round(mean_bui*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 245), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = -7, y = 225, colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = logpct_95th), data = subset(mx_stats, region == 'East'), 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = subset(mx_stats, region == 'East'),
            aes(label = paste(formatC(round(pct_95th*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 210), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")

firefreq_west <- as.data.frame(extraction_df) %>%
  filter(region ==  "West") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  #transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  ggtitle("West") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(-8, 8)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = -7, y = 200, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == '3'), 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = subset(mx_info, PANEL == '3'),
            aes(label=paste(formatC(round(exp(x)*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 185), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = -7, y = 160, colour = "blue", size = 5) +
  geom_vline(aes(xintercept = logmean_bui), data = subset(mx_stats, region == 'West'), 
             linetype = "dashed", color  = "blue") +
  geom_text(data = subset(mx_stats, region == 'West'),
            aes(label = paste(formatC(round(mean_bui*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 145), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = -7, y = 125, colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = logpct_95th), data = subset(mx_stats, region == 'West'), 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = subset(mx_stats, region == 'West'),
            aes(label = paste(formatC(round(pct_95th*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 110), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")

g <- arrangeGrob(firefreq_east, firefreq_cent, firefreq_west, ncol = 3)

ggsave("results/region/bui/bui_hist_region.pdf", g, width = 8, height = 4, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))
