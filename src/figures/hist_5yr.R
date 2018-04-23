
# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., year = '1990', panel = '1')

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., year = '1995', panel = '2')

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., year = '2000', panel = '3')

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., year = '2005', panel = '4')

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., year = '2010', panel = '5')

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., year = '2015', panel = '6')

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/bui/bui_hist_per5yr.pdf", g, width = 12, height = 4, dpi=600, scale = 3, units = "cm") #saves g



