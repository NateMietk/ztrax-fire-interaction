
if (exists('fpa_to_buffer')) {
  fpa_to_buffer <- st_read(file.path(fpa_out, 'fpa_mtbs_bae.gpkg')) %>%
    dplyr::select(FPA_ID) %>%
    st_cast("POLYGON")
}

if (!file.exists(file.path(fpa_out, 'fpa_buffer_1k.gpkg'))) {
  sfInit(parallel = TRUE, cpus = parallel::detectCores()/4)
  sfExport(list = c('fpa_to_buffer'))

  fpa_1k <- sfLapply(unique(fpa_to_buffer$FPA_ID),
                     fun = st_multibuffer,
                     data = fpa_to_buffer)
  sfStop()
  test <- do.call(rbind, fpa_1k)

  st_write(mtbs_fire, file.path(fpa_out, "fpa_buffer_1k.gpkg"),
           driver = "GPKG", delete_layer = TRUE)

  system(paste0("aws s3 sync ", prefix, " ", s3_base))

} else {
  fpa_1k <- st_read(file.path(fpa_out, "fpa_buffer_1k.gpkg"))
}
























rm(fpa)

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c('fpa_1k'))

fpa_2k <- sfLapply(unique(fpa_1k$FPA_ID),
                   fun = st_multibuffer,
                   data = fpa_1k,
                   varname = 'fpa_buffer_2k')
sfStop()
rm(fpa_1k)

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c('fpa_2k'))

fpa_3k <- sfLapply(unique(fpa_2k$FPA_ID),
                   fun = st_multibuffer,
                   data = fpa_2k,
                   varname = 'fpa_buffer_3k')
sfStop()
rm(fpa_2k)

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c('fpa_3k'))

fpa_4k <- sfLapply(unique(fpa_3k$FPA_ID),
                   fun = st_multibuffer,
                   data = fpa_3k,
                   varname = 'fpa_buffer_4k')
sfStop()
rm(fpa_3k)

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c('fpa_4k'))

fpa_5k <- sfLapply(unique(fpa_4k$FPA_ID),
                   fun = st_multibuffer,
                   data = fpa_4k,
                   varname = 'fpa_buffer_5k')
sfStop()
rm(fpa_4k)

sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c('fpa_5k'))

fpa_10k <- sfLapply(unique(fpa_5k$FPA_ID),
                    fun = st_multibuffer,
                    data = fpa_5k,
                    varname = 'fpa_buffer_10k')
sfStop()
rm(fpa_5k)
rm(fpa_10k)

}
