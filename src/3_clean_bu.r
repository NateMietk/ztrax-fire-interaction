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
  bu_stack <- velox(stack(bu_list))
}

ecoregions <- ecoreg_plain %>%
  group_by(us_l3name) %>%
  summarise() %>%
  st_cast('POLYGON')

# What are the ecoreg level built-up intensity per 5 year incriments
if (!exists('sum_extractions_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'sum_ecoregion_bu.rds'))) {

    sum_extractions_bu <- bu_stack$extract(sp = ecoregions, fun = function(x) {sum(x, na.rm=TRUE)}) %>%
      as_tibble() %>%
      mutate(ecoregion_id = data.frame(ecoregions_mini)$us_l3name,
             sum_bu_1990 = V1,
             sum_bu_1995 = V2,
             sum_bu_2000 = V3,
             sum_bu_2005 = V4,
             sum_bu_2010 = V5,
             sum_bu_2015 = V6) %>%
      dplyr::select(-starts_with('V'))

    write_rds(sum_extractions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'sum_ecoregion_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  sum_ecoregions_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'sum_ecoregion_bu.rds'))
}

# What are the ecoreg level built-up intensity per 5 year incriments
if (!exists('mean_extractions_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'mean_ecoregion_bu.rds'))) {

    bu_stack <- velox(stack(bu_list))
    mean_extractions_bu <- bu_stack$extract(sp = ecoregions, fun = function(x) {mean(x, na.rm=TRUE)}) %>%
      as_tibble() %>%
      mutate(ecoregion_id = data.frame(ecoregions)$us_l3name,
             mean_bu_1990 = V1,
             mean_bu_1995 = V2,
             mean_bu_2000 = V3,
             mean_bu_2005 = V4,
             mean_bu_2010 = V5,
             mean_bu_2015 = V6) %>%
      dplyr::select(-starts_with('V'))

    write_rds(mean_extractions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'mean_ecoregion_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  mean_ecoregions_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'mean_ecoregion_bu.rds'))
}

if (!exists('median_extractions_bu')) {
  if (!file.exists(file.path(anthro_out, 'building_counts', 'building_counts_all', 'median_ecoregion_bu.rds'))) {

    median_extractions_bu <- bu_stack$extract(sp = ecoregions, fun = function(x){median(x, na.rm=TRUE)}) %>%
      as_tibble() %>%
      mutate(ecoregion_id = data.frame(ecoregions)$us_l3name,
             median_bu_1990 = V1,
             median_bu_1995 = V2,
             median_bu_2000 = V3,
             median_bu_2005 = V4,
             median_bu_2010 = V5,
             median_bu_2015 = V6) %>%
      dplyr::select(-starts_with('V'))

    write_rds(median_extractions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'median_ecoregion_bu.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  median_ecoregions_bu <- read_rds(file.path(anthro_out, 'building_counts', 'building_counts_all', 'median_ecoregion_bu.rds'))
}

# setup parallel environment
sum_extractions_bu <- bu_stack$extract(sp = fpa, fun = function(x){sum(x, na.rm=TRUE)}) %>%
  as_tibble() %>%
  mutate(fpa_id = as.data.frame(fpa)$FPA_ID,
         sum_bu_1990 = V1,
         sum_bu_1995 = V2,
         sum_bu_2000 = V3,
         sum_bu_2005 = V4,
         sum_bu_2010 = V5,
         sum_bu_2015 = V6) %>%
  dplyr::select(-starts_with('V'))
write_rds(sum_extractions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'sum_fpa_perimeter_bu.rds'))
system(paste0("aws s3 sync ", prefix, " ", s3_base))

# setup parallel environment
mean_extractions_bu <- bu_stack$extract(sp = fpa, fun = function(x){mean(x, na.rm=TRUE)}) %>%
  as_tibble() %>%
  mutate(fpa_id = as.data.frame(fpa)$FPA_ID,
         mean_bu_1990 = V1,
         mean_bu_1995 = V2,
         mean_bu_2000 = V3,
         mean_bu_2005 = V4,
         mean_bu_2010 = V5,
         mean_bu_2015 = V6) %>%
  dplyr::select(-starts_with('V'))
write_rds(mean_extractions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'mean_fpa_perimeter_bu.rds'))
system(paste0("aws s3 sync ", prefix, " ", s3_base))

# setup parallel environment
median_extractions_bu <- bu_stack$extract(sp = fpa, fun = function(x){median(x, na.rm=TRUE)}) %>%
  as_tibble() %>%
  mutate(fpa_id = as.data.frame(fpa)$FPA_ID,
         median_bu_1990 = V1,
         median_bu_1995 = V2,
         median_bu_2000 = V3,
         median_bu_2005 = V4,
         median_bu_2010 = V5,
         median_bu_2015 = V6) %>%
  dplyr::select(-starts_with('V'))
write_rds(median_extractions_bu, file.path(anthro_out, 'building_counts', 'building_counts_all', 'median_fpa_perimeter_bu.rds'))
system(paste0("aws s3 sync ", prefix, " ", s3_base))
