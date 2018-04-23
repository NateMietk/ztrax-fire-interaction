

# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '1990', panel = '1', 
            upper = 125, conus = FALSE)

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '1995', panel = '2', 
            upper = 125, conus = FALSE)

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2000', panel = '3', 
            upper = 125, conus = FALSE)

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2005', panel = '4', 
            upper = 125, conus = FALSE)

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2010', panel = '5', 
            upper = 125, conus = FALSE)

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2015', panel = '6', 
            upper = 125, conus = FALSE)

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/region/bui/bui_east_hist_per5yr.pdf", g, width = 18, height = 5, dpi=600, scale = 3, units = "cm") #saves g


# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West',
            year = '1990', panel = '1')

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West',
            year = '1995', panel = '2')

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West',
            year = '2000', panel = '3')

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West',
            year = '2005', panel = '4')

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West',
            year = '2010', panel = '5')

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West',
            year = '2015', panel = '6')

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/region/bui/bui_west_hist_per5yr.pdf", g, width = 18, height = 5, dpi=600, scale = 3, units = "cm") #saves g

firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '1990', panel = '1', upper = 100)

firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '1995', panel = '2', upper = 100)

firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2000', panel = '3', upper = 100)

firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2005', panel = '4', upper = 100)

firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2010', panel = '5', upper = 100)

firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2015', panel = '6', upper = 100)

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/region/bui/bui_central_hist_per5yr.pdf", g, width = 18, height = 5, dpi=600, scale = 3, units = "cm") #saves g

