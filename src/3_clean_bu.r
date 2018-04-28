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
                        pattern = glob2rx('*masked*tif'),
                        full.names = TRUE)
  bu <- do.call(brick, lapply(bu_list, raster))
}

# What are the state and CONUS level building counts per 5 year incriments
if (!exists('usa_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))) {

    sfInit(parallel = TRUE, cpus = parallel::detectCores())
    sfExport(list = c('usa_shp'))
    sfSource('src/functions/helper_functions.R')

    states_bu <- sfLapply(bu_list, fun = extract_one,
                           use_varname = TRUE, varname = 'bu_state_',
                           shapefile_extractor = usa_shp,
                           prefix = prefix, s3_base = s3_base)

    sfStop()

    state_bu_df <- states_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(state_id = data.frame(usa_shp)$stusps)

    write_rds(state_bu_df, file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))
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

    sfInit(parallel = TRUE, cpus = parallel::detectCores())
    sfExport(list = c('regions'))
    sfSource('src/functions/helper_functions.R')

    regions_bu <- sfLapply(bu_list, fun = extract_one,
                            use_varname = TRUE, varname = 'bu_regional_',
                            shapefile_extractor = regions,
                            prefix = prefix, s3_base = s3_base)

    sfStop()

    regions_bu_df <- regions_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(region_id = data.frame(regions)$region)


    write_rds(regions_bu_df, file.path(anthro_out, 'building_counts', 'building_counts_all', 'region_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  }
} else {
  usa_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'usa_bu.rds'))
}

# What are the ecoreg level built-up intensity per 5 year incriments
if (!exists('ecoreg_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'ecoregion_bu.rds'))) {

    ecoregions <- ecoreg_plain %>%
      group_by(us_l3name) %>%
      summarise()

    sfInit(parallel = TRUE, cpus = parallel::detectCores())
    sfExport(list = c('ecoregions'))
    sfSource('src/functions/helper_functions.R')

    ecoregions_bu <- sfLapply(bu_list, fun = extract_one,
                               use_varname = TRUE, varname = 'bu_ecoregion_',
                               shapefile_extractor = ecoregions,
                               prefix = prefix, s3_base = s3_base)

    sfStop()

    ecoregions_bu_df <- ecoregions_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      mutate(ecoregion_id = data.frame(ecoregions)$us_l3name)

    write_rds(ecoregions_bu_df, file.path(anthro_out, 'building_counts', 'building_counts_all', 'ecoregion_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  }
} else {
  ecoregions_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'ecoregion_bu.rds'))
}

# setup parallel environment
sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c("fpa"))

extractions_bu <- sfLapply(as.list(bu_list),
                            fun = extract_one,
                            shapefile_extractor = fpa)
sfStop()
