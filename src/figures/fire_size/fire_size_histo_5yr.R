
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
