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
  bu_stack <- stack(bu_list)
}

# Housing units per ecoregion
if (!exists('sum_extractions_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_ecoregion_bu.rds'))) {

    ecoregions <- ecoreg_plain %>%
      group_by(us_l3name) %>%
      summarise() %>%
      st_cast('POLYGON')

    library(snow)
    beginCluster(n = 10)
    sum_ecoregions_bu <-
      raster::extract(bu_stack,
                      ecoregions,
                      na.rm = TRUE,
                      stat = 'sum',
                      df = TRUE)
    endCluster()

    sum_ecoregions_count <- sum_ecoregions_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      dplyr::count()

    sum_ecoregions_bu <- sum_ecoregions_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      left_join(., sum_ecoregions_count, by = 'ID') %>%
      mutate(ecoregion_id = data.frame(ecoregions)$us_l3name)

    write_rds(sum_ecoregions_bu,
              file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_ecoregion_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  sum_ecoregions_bu <-
    read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_ecoregion_bu.rds'))
}

# Housing units per FPA buffered point
if (!exists('sum_fpa_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_fpa_bu.rds'))) {

    sum_fpa_bu <-
      raster::extract(bu_stack,
                      fpa,
                      na.rm = TRUE,
                      stat = 'sum',
                      df = TRUE)

    sum_fpa_bu_count <- sum_fpa_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      dplyr::count()

    sum_fpa_bu <- sum_fpa_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      left_join(., sum_fpa_bu_count, by = 'ID') %>%
      mutate(FPA_ID = data.frame(fpa)$FPA_ID)

    write_rds(sum_fpa_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_fpa_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  sum_fpa_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_fpa_bu.rds'))
}

# Housing units per fpa_1k buffered point
if (!exists('sum_fpa_1k_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_fpa_1k_bu.rds'))) {

    # library(snow)
    # beginCluster(n = 1)
    sum_fpa_1k_bu <-
      raster::extract(bu_stack,
                      fpa_1k,
                      na.rm = TRUE,
                      stat = 'sum',
                      df = TRUE)
    # endCluster()

    sum_fpa_1k_bu_count <- sum_fpa_1k_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      dplyr::count()

    sum_fpa_1k_bu <- sum_fpa_1k_bu %>%
      bind_cols %>%
      as_tibble %>%
      mutate(index = ID) %>%
      dplyr::select(-starts_with("ID")) %>%
      dplyr::select(-starts_with("X")) %>%
      rename(ID = index) %>%
      group_by(ID) %>%
      summarise_all(funs(sum), na.rm = TRUE) %>%
      left_join(., sum_fpa_bu_count, by = 'ID') %>%
      mutate(FPA_ID = data.frame(fpa)$FPA_ID)

    write_rds(sum_fpa_1k_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_fpa_1k_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  sum_fpa_1k_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'summary_fpa_1k_bu.rds'))
}
