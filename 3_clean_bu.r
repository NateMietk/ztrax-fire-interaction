# Import the buidling units data
if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'bu_masked_1990.tif'))) {
  bu_list <- list.files(file.path(anthro_out, 'building_counts', 'building_counts_all'),
                         pattern = 'temp_slice',
                         full.names = TRUE)
  bu <- do.call(brick, lapply(bu_list, raster))
  bu_masked <- mask(bu, fbuy_raster)

  year_seq <- c(1990, 1995, 2000, 2005, 2010, 2015)

  bu_masked <- setZ(bu_masked, year_seq, 'years')
  names(bu_masked) <- paste0("bu_masked_", year_seq)

  writeRaster(bu_masked,
              filename = file.path(anthro_out, 'building_counts', 'building_counts_all', names(bu_masked)),
              bylayer=TRUE, format="GTiff")
  system(paste0("aws s3 sync ", prefix, " ", s3_base))

} else {

  bu_list <- list.files(file.path(anthro_out, 'building_counts', 'building_counts_all'),
                         pattern = 'masked',
                         full.names = TRUE)
  bu <- do.call(brick, lapply(bu_list, raster))
}

# What are the state and CONUS level building counts per 5 year incriments
if (!exists('usa_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))) {

    library(snow)
    beginCluster(n = cores)
    usa_bu <- raster::extract(bu, usa_shp, na.rm = TRUE, stat = 'sum', df = TRUE)
    endCluster()

    usa_bu <- usa_bu %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(fpa_id = data.frame(usa_shp)$stusps)

    write_rds(usa_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  }
} else {
  usa_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))
}

# What are the regional (west, central, east) level building counts per 5 year incriments
if (!exists('region_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'region_bu.rds'))) {

    regions <- ecoregl1 %>%
      group_by(region) %>%
      summarise()

    library(snow)
    beginCluster(n = cores)
    regions_bu <- raster::extract(bu, regions, na.rm = TRUE, stat = 'sum', df = TRUE)
    endCluster()

    regions_bu <- regions_bu %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(fpa_id = data.frame(ecoregl1)$region)

    write_rds(regions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'region_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  }
} else {
  usa_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))
}

# setup parallel environment
sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c("fpa"))

extractions_bu <- sfLapply(as.list(bu_list),
                            fun = extract_one,
                            shapefile_extractor = fpa)
sfStop()
