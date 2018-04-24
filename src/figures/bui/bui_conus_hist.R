# Plot the histogram of built-up area
# The burned area/fire was long tailed (range 0.01-3000) - had to log transform
p4 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  theme_pub()

mx_cnt <- ggplot_build(p4)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(y, x, count) %>%
  mutate(max_count = max(count)) %>%
  filter(y == max_count)

mx_stats <- as.data.frame(extraction_df) %>%
  group_by() %>%
  mutate(bui_ha = BUI*0.0001000000884,
         log_bui_ha = log(bui_ha)) %>%
  summarise(mean_bui = mean(bui_ha),
            pct_95th = quantile(bui_ha, probs = 0.95),
            logmean_bui = log(mean_bui),
            logpct_95th = log(pct_95th)) %>%
  ungroup()


p4 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 650)) +
  scale_x_continuous(limits = c(-8, 8)) +
  theme_pub() +
  geom_text(aes(label = 'Peak:'), x = -7, y = 635, colour = "darkred", size = 5) +
  geom_vline(aes(xintercept = x), data = mx_cnt, 
             linetype = "dashed", color  = "darkred") +
  geom_text(data = mx_cnt,
            aes(label=paste(formatC(round(exp(x)*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 600), colour = "darkred", size = 4) +
  
  geom_text(aes(label = 'Mean:'), x = -7, y = 575, colour = "blue", size = 5) +
  geom_vline(aes(xintercept = logmean_bui), data = mx_stats, 
             linetype = "dashed", color  = "blue") +
  geom_text(data = mx_stats,
            aes(label = paste(formatC(round(mean_bui*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 540), colour = "blue", size = 4) +
  
  geom_text(aes(label = '95th:'), x = -7, y = 515, colour = "darkgreen", size = 5) +
  geom_vline(aes(xintercept = logpct_95th), data = mx_stats, 
             linetype = "dashed", color  = "darkgreen") +
  geom_text(data = mx_stats,
            aes(label = paste(formatC(round(pct_95th*10000, 1), format="f", big.mark=",", digits=1), "sq m"), 
                x = -5, y = 480), colour = "darkgreen", size = 4) +
  theme(legend.position = "none")


ggsave("results/bui/bui_hist.pdf", p4, width = 5, height = 4, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))

