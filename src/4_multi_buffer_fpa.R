
if (!exists('fpa')) {
  fpa <- st_read(file.path(fpa_out, 'fpa_mtbs_bae.gpkg'))
}


if (!file.exists(file.path(fpa_out, 'fpa_buffer_1k.gpkg'))) {

  fpa_1k <- fpa %>%
    st_buffer(., dist = 1000)

  st_write(fpa_1k, file.path(fpa_out, "fpa_buffer_1k.gpkg"),
           driver = "GPKG", delete_layer = TRUE)

  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  rm(fpa)

} else {
  fpa_1k <- st_read(file.path(fpa_out, "fpa_buffer_1k.gpkg"))
}

if (!file.exists(file.path(fpa_out, 'fpa_buffer_2k.gpkg'))) {

  fpa_2k <- st_buffer(fpa_1k, dist = 1000)

  st_write(fpa_2k, file.path(fpa_out, "fpa_buffer_2k.gpkg"),
           driver = "GPKG", delete_layer = TRUE)

  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  rm(fpa_1k)

} else {
  fpa_2k <- st_read(file.path(fpa_out, "fpa_buffer_2k.gpkg"))
}


if (!file.exists(file.path(fpa_out, 'fpa_buffer_3k.gpkg'))) {

  fpa_3k <- st_buffer(fpa_2k, dist = 1000)

  st_write(fpa_3k, file.path(fpa_out, "fpa_buffer_3k.gpkg"),
           driver = "GPKG", delete_layer = TRUE)

  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  rm(fpa_2k)

} else {
  fpa_3k <- st_read(file.path(fpa_out, "fpa_buffer_3k.gpkg"))
}

if (!file.exists(file.path(fpa_out, 'fpa_buffer_4k.gpkg'))) {

  fpa_4k <- st_buffer(fpa_3k, dist = 1000)

  st_write(fpa_4k, file.path(fpa_out, "fpa_buffer_4k.gpkg"),
           driver = "GPKG", delete_layer = TRUE)

  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  rm(fpa_3k)

} else {
  fpa_4k <- st_read(file.path(fpa_out, "fpa_buffer_4k.gpkg"))
}
