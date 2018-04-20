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

extract_one <- function(filename, shapefile_extractor, prefix = prefix, s3_base = s3_base) {
  # function to extract all climate time series based on shapefile input
  # this results in large list of all months/years within the raster climate data
  # each list is written out to a csv so this only needs to be run once.
  # inputs:
  # filename -> a list of all tif filenames with full path
  # shapefile_extractor -> the shapefile (point or polygon) to extract climate data

  out_name <- gsub('.tif', '.csv', filename)
  if (!file.exists(out_name)) {
    res <- raster::extract(raster::raster(filename), shapefile_extractor,
                           na.rm = TRUE, stat = 'sum', df = TRUE)
    write.csv(res, file = out_name)

    system(paste0("aws s3 sync ",
                  prefix, " ",
                  s3_base))
  } else {
    res <- read.csv(out_name)
  }
  res
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
