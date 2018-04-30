
# Import the FBUY data
if (!exists('fbuy_extractions')) {
  if( !file.exists('data/anthro/first_year_built/FBUY/FBUY.csv')) {
    filename <- file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif')
    out_name <- gsub('.tif', '.csv', filename)

    fbuy_extractions <- raster(file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif'))
    fbuy_extractions <- extract(fbuy_extractions, mtbs_fire, na.rm = TRUE, stat = 'sum', df = TRUE)
    write_csv(fbuy_extractions, file = out_name)

    system(paste0("aws s3 sync ", prefix, " ", s3_base))

  } else {
    fbuy_raster <- raster(file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif'))

    fbuy_extractions <- read_csv('data/anthro/first_year_built/FBUY/FBUY.csv')
  }
}

fbuy_raster[fbuy_raster == 1] <- NA
