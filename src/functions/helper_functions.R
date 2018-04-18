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

extract_one <- function(filename, shapefile_extractor) {
  # function to extract all climate time series based on shapefile input
  # this results in large list of all months/years within the raster climate data
  # each list is written out to a csv so this only needs to be run once.
  # inputs:
  # filename -> a list of all tif filenames with full path
  # shapefile_extractor -> the shapefile (point or polygon) to extract climate data
  
  out_name <- gsub('.tif', '.csv', filename)
  if (!file.exists(out_name)) {
    res <- raster::extract(raster::stack(filename), shapefile_extractor,
                           na.rm = TRUE, fun = 'sum', df = TRUE)
    write.csv(res, file = out_name)
  } else {
    res <- read.csv(out_name)
  }
  res
}