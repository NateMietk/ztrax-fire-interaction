
# check to see where the min. diffs fall in plot
east_firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '1990', panel = '1', 
            upper = 100, conus = FALSE)

east_firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '1995', panel = '2', 
            upper = 100, conus = FALSE)

east_firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2000', panel = '3', 
            upper = 100, conus = FALSE)

east_firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2005', panel = '4', 
            upper = 100, conus = FALSE)

east_firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2010', panel = '5', 
            upper = 100, conus = FALSE)

east_firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'East',
            year = '2015', panel = '6', 
            upper = 100, conus = FALSE)

g1 <- arrangeGrob(east_firefreq_90, east_firefreq_95, east_firefreq_00, east_firefreq_05, 
                 east_firefreq_10, east_firefreq_15, nrow = 1)
ggsave("results/region/bui/bui_east_hist_per5yr.pdf", g1, width = 12, height = 5, dpi=600, scale = 4, units = "cm") #saves g


# check to see where the min. diffs fall in plot
west_firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West', upper = 250,
            year = '1990', panel = '1')

west_firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West', upper = 250,
            year = '1995', panel = '2')

west_firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West', upper = 250,
            year = '2000', panel = '3')

west_firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West', upper = 250,
            year = '2005', panel = '4')

west_firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West', upper = 250,
            year = '2010', panel = '5')

west_firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'West', upper = 250,
            year = '2015', panel = '6')

g2 <- arrangeGrob(west_firefreq_90, west_firefreq_95, west_firefreq_00, 
                 west_firefreq_05, west_firefreq_10, west_firefreq_15, nrow = 1)
ggsave("results/region/bui/bui_west_hist_per5yr.pdf", g2, width = 12, height = 5, dpi=600, scale = 4, units = "cm") #saves g

central_firefreq_90 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '1990', panel = '1', upper = 250)

central_firefreq_95 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '1995', panel = '2', upper = 250)

central_firefreq_00 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2000', panel = '3', upper = 250)

central_firefreq_05 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2005', panel = '4',upper = 250)

central_firefreq_10 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2010', panel = '5', upper = 250)

central_firefreq_15 <- as.data.frame(extraction_df) %>%
  hist_plot(., regions = 'Central',
            year = '2015', panel = '6', upper = 250)

g3 <- arrangeGrob(central_firefreq_90, central_firefreq_95, central_firefreq_00, 
                 central_firefreq_05, central_firefreq_10, central_firefreq_15, nrow = 1)
ggsave("results/region/bui/bui_central_hist_per5yr.pdf", g3, width = 12, height = 5, dpi=600, scale = 4, units = "cm") #saves g

g <- arrangeGrob(east_firefreq_90, east_firefreq_95, east_firefreq_00, east_firefreq_05, east_firefreq_10, east_firefreq_15,
                 central_firefreq_90, central_firefreq_95, central_firefreq_00, central_firefreq_05, central_firefreq_10, central_firefreq_15,
                 west_firefreq_90, west_firefreq_95, west_firefreq_00, west_firefreq_05, west_firefreq_10, west_firefreq_15, nrow = 3)
ggsave("results/region/bui/bui_allregions_hist_per5yr.pdf", g, width = 14, height = 12, dpi=600, scale = 4, units = "cm") #saves g
