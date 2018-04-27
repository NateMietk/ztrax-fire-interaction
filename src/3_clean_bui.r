# Import the BUI data
if (!file.exists(file.path(anthro_out, 'built_up_intensity', 'BUI', 'bui_masked_1990.tif'))) {

  bui_list <- list.files(file.path(anthro_out, 'built_up_intensity', 'BUI'),
                         pattern = '.tif',
                         full.names = TRUE)
  bui <- do.call(brick, lapply(bui_list, raster))
  bui_masked <- mask(bui, fbuy_raster)

  # Modis date creation
  year_seq <- c(1990, 1995, 2000, 2005, 2010, 2015)

  bui_masked <- setZ(bui_masked, year_seq, 'years')
  names(bui_masked) <- paste0("bui_masked_", year_seq)

  writeRaster(bui_masked,
              filename = file.path(anthro_out, 'built_up_intensity', 'BUI', names(bui_masked)),
              bylayer=TRUE, format="GTiff")
  system(paste0("aws s3 sync ", prefix, " ", s3_base))

} else {

  bui_list <- list.files(file.path(anthro_out, 'built_up_intensity', 'BUI'),
                         pattern = glob2rx('*masked*tif'),
                         full.names = TRUE)
  bui <- do.call(brick, lapply(bui_list, raster))
}

# What are the state and CONUS level built-up intensity per 5 year incriments
if (!exists('usa_bui')) {
  if (!file.exists(file.path(anthro_out, 'built_up_intensity', 'BUI', 'usa_bui.rds'))) {

    sfInit(parallel = TRUE, cpus = parallel::detectCores())
    sfExport(list = c('usa_shp'))
    sfSource('src/functions/helper_functions.R')
    
    regions_bui <- sfLapply(bui_list, fun = extract_one,
                            shapefile_extractor = usa_shp, 
                            prefix = prefix, s3_base = s3_base,
                            stack = FALSE)
    
    sfStop()

    usa_bui <- usa_bui %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(fpa_id = data.frame(usa_shp)$stusps)

    write_rds(usa_bui, file.path(anthro_out, 'built_up_intensity', 'BUI', 'usa_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  }
} else {
  usa_bui <- read_rds(file.path(anthro_out, 'built_up_intensity', 'BUI', 'usa_bui.rds'))
}

# What are the regional (west, central, east) level built-up intensity per 5 year incriments
if (!exists('region_bui')) {
  if (!file.exists(file.path(anthro_out, 'built_up_intensity', 'BUI', 'region_bui.rds'))) {

    regions <- ecoregl1 %>%
      group_by(region) %>%
      summarise()

    sfInit(parallel = TRUE, cpus = parallel::detectCores())
    sfExport(list = c('regions'))
    sfSource('src/functions/helper_functions.R')
    
    regions_bui <- sfLapply(bui_list, fun = extract_one,
                            usevarname = TRUE, varname = 'bui_regional_',
                            shapefile_extractor = regions, 
                            prefix = prefix, s3_base = s3_base)
    
    sfStop()
    
    regions_bui <- regions_bui %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      rename(ID = index)
    
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(fpa_id = data.frame(ecoregl1)$region)

    write_rds(regions_bui, file.path(anthro_out, 'built_up_intensity', 'BUI', 'region_bui.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  }
} else {
  regions_bui <- read_rds(file.path(anthro_out, 'built_up_intensity', 'BUI', 'region_bui.rds'))
}


# setup parallel environment
sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c("fpa"))

extractions <- sfLapply(as.list(bui_list),
                        fun = extract_one,
                        shapefile_extractor = fpa)
sfStop()