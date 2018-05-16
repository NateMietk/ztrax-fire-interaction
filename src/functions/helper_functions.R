load_data <- function(url, dir, layer, outname) {
  file <- paste0(dir, "/", layer, ".shp")

  if (!file.exists(file)) {
    download.file(url, destfile = paste0(dir, ".zip"))
    unzip(paste0(dir, ".zip"),
          exdir = dir)
    unlink(paste0(dir, ".zip"))

  }
  name <- paste0(outname, "_shp")
  name <- sf::st_read(dsn = dir, layer = layer)
  name
}

st_multibuffer <- function (ids, data) {
  require(sf)
  require(tidyverse)

  df <- subset(data, data$FPA_ID == ids) %>%
    dplyr::select(FPA_ID)

  fpa_buffer <- df %>%
    mutate(ring = st_buffer(geometry, 1000)) %>%
    group_by(FPA_ID) %>%
    mutate(buffer_distance = 1000,
           geometry = st_difference(ring, geometry)) %>%
    ungroup()

  fpa_buffer

}

extract_one <- function(filename, shapefile_extractor,
                        use_varname = TRUE, varname,
                        prefix = prefix, s3_base = s3_base) {
  # function to extract all climate time series based on shapefile input
  # this results in large list of all months/years within the raster climate data
  # each list is written out to a csv so this only needs to be run once.
  # inputs:
  # filename -> a list of all tif filenames with full path
  # shapefile_extractor -> the shapefile (point or polygon) to extract climate data
  require(tidyverse)
  require(raster)

  if (use_varname == TRUE) {

    file_split <- filename %>%
      basename %>%
      strsplit(split = "_") %>%
      unlist
    year <- file_split[max(length(file_split))]
    dir_name <- dirname(filename)

    out_name <- paste0(dir_name, '/', varname, year)
    out_name <- gsub('.tif', '.csv', out_name)
    out_name

    } else {
      out_name <- gsub('.tif', '.csv', filename)
      out_name

    }
  }

classify_fire_size <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x >= 500 & x < 5000, "500 - 5000",
         ifelse(x >= 5000 & x < 25000, "5000 - 25000",
                ifelse(x >= 25000 & x < 50000, "25000 - 50000",
                       "> 50000")))
}
# GGPLOT Theme ------------------------------------------------------------
theme_pub <- function(base_size=14, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 14),

            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),

            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),

            legend.title = element_text(size=14),
            legend.position = "right",
            legend.text = element_text(size=14),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "transparent"),

            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 14),

            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 14, angle = 65, hjust = 1),
            axis.text.y = element_text(size = 14)))
}

hist_plot <- function(df, regions, year, panel, upper = 250, conus = TRUE) {

  if(conus == FALSE) {
    p_df <- df %>%
      filter(region != as.character(regions)) %>%
      mutate(bui_ha = BUI*0.0001000000884,
             mean_bui = mean(BUI*0.0001000000884))
  } else {
    p_df <- df %>%
      mutate(bui_ha = BUI*0.0001000000884,
             mean_bui = mean(BUI*0.0001000000884))
  }

  p_template <- p_df %>%
    ggplot(aes(x = log(bui_ha))) +
    geom_histogram(binwidth = 0.25) +
    theme_pub() +
    facet_wrap(~fire_bidecadal)

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

  mx_stats <- mx_info %>%
    mutate(fire_bidecadal = sort(unique(p_df$fire_bidecadal))) %>%
    right_join(., p_df, by = "fire_bidecadal") %>%
    dplyr::select(fire_bidecadal, x, y, BUI) %>%
    group_by(fire_bidecadal) %>%
    mutate(bui_ha = BUI*0.0001000000884,
           log_bui_ha = log(bui_ha)) %>%
    summarise(mean_bui = mean(bui_ha),
              pct_95th = quantile(bui_ha, probs = 0.95),
              logmean_bui = log(mean_bui),
              logpct_95th = log(pct_95th)) %>%
    ungroup()

  p <- p_df %>%
    filter(fire_bidecadal == as.numeric(year)) %>%
    mutate(bui_ha = BUI*0.0001000000884,
           mean_bui = mean(BUI*0.0001000000884)) %>%
    ggplot(aes(x = log(bui_ha))) +
    geom_histogram(binwidth = 0.5) +
    ylab("Counts") +
    xlab('log Built-up Intentsity (ha)') +
    scale_y_continuous(limits = c(0, upper)) +
    scale_x_continuous(limits = c(-8, 8)) +
    ggtitle(year) +
    theme_pub() +
    geom_text(aes(label = 'Peak:'), x = -7, y = upper, colour = "darkred", size = 5) +
    geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == as.character(panel)),
               linetype = "dashed", color  = "darkred") +
    geom_text(data=subset(mx_info, PANEL == as.character(panel)),
              aes(label=paste(formatC(round(exp(x)*10000, 1), format="f", big.mark=",", digits=1), "sq m"),
                  x = -5, y = upper - upper*0.056), colour = "darkred", size = 4) +

    geom_text(aes(label = 'Mean:'), x = -7, y = upper - upper*0.12, colour = "blue", size = 5) +
    geom_vline(aes(xintercept = logmean_bui), data = subset(mx_stats, fire_bidecadal ==  year),
               linetype = "dashed", color  = "blue") +
    geom_text(data = subset(mx_stats, fire_bidecadal ==  year),
              aes(label = paste(formatC(round(mean_bui*10000, 1), format="f", big.mark=",", digits=1), "sq m"),
                  x = -5, y = upper - upper*0.176), colour = "blue", size = 4) +

    geom_text(aes(label = '95th:'), x = -7, y = upper - upper*0.28, colour = "darkgreen", size = 5) +
    geom_vline(aes(xintercept = logpct_95th), data = subset(mx_stats, fire_bidecadal ==  year),
               linetype = "dashed", color  = "darkgreen") +
    geom_text(data = subset(mx_stats, fire_bidecadal ==  year),
              aes(label = paste(formatC(round(pct_95th*10000, 1), format="f", big.mark=",", digits=1), "sq m"),
                  x = -5, y = upper - upper*0.336), colour = "darkgreen", size = 4) +
    theme(legend.position = "none")
  p
}

hist_fbuy_plot <- function(df, regions, panel, upper = 250) {

  p_template <- df %>%
    filter(fbuy_mean != 0) %>%
    ggplot(aes(x = fbuy_mean)) +
    geom_histogram(binwidth = 5) +
    theme_pub()

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

  mx_stats <- df %>%
    filter(fbuy_mean != 0) %>%
    group_by(region) %>%
    summarise(mean_fbuy = mean(fbuy_mean),
              median_fbuy = median(fbuy_mean),
              pct_95th = quantile(fbuy_mean, probs = 0.95)) %>%
    ungroup()

  p <- df %>%
    filter(fbuy_mean != 0) %>%
    ggplot(aes(x = fbuy_mean)) +
    geom_histogram(binwidth = 1) +
    ylab("Counts") +
    xlab('Mean FBUY') +
    scale_y_continuous(limits = c(0, upper)) +
    scale_x_continuous(limits = c(1850, 2015)) +
    ggtitle(regions) +
    theme_pub() +
    geom_text(aes(label = 'Peak:'), x = 1875, y = upper, colour = "darkred", size = 5) +
    geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == as.character(panel)),
               linetype = "dashed", color  = "darkred") +
    geom_text(data=subset(mx_info, PANEL == as.character(panel)),
              aes(label=round(x, 0),
                  x = 1865, y = upper - upper*0.056), colour = "darkred", size = 4) +

    geom_text(aes(label = 'Mean:'), x = 1875, y = upper - upper*0.12, colour = "blue", size = 5) +
    geom_vline(aes(xintercept = mean_fbuy), data = mx_stats,
               linetype = "dashed", color  = "blue") +
    geom_text(data = mx_stats,
              aes(label = round(mean_fbuy, 0),
                  x = 1865, y = upper - upper*0.176), colour = "blue", size = 4) +

    geom_text(aes(label = '95th:'), x = 1875, y = upper - upper*0.28, colour = "darkgreen", size = 5) +
    geom_vline(aes(xintercept = pct_95th), data = mx_stats,
               linetype = "dashed", color  = "darkgreen") +
    geom_text(data = mx_stats,
              aes(label = round(pct_95th, 0),
                  x = 1865, y = upper - upper*0.336), colour = "darkgreen", size = 4) +
    theme(legend.position = "none")
  p
}
