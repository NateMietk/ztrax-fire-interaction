# Libraries ---------------------------------------------------------------
x <- c("tidyverse", "magrittr", "sf", "gridExtra", "rgdal", "raster", "rgeos", "lwgeom", 'snowfall',
       "assertthat", "purrr", "httr", "rvest", "lubridate", "doParallel", "RColorBrewer", "ggthemes")
lapply(x, library, character.only = TRUE, verbose = FALSE)

proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
source('src/functions/helper_functions.R')

cores <- detectCores()

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ecoregion_prefix <- file.path(raw_prefix, "ecoregions")
ecoregion_out <- file.path(ecoregion_prefix, "us_eco_l3")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")

# Cleaned data output folders
anthro_out <- file.path(prefix, "anthro")
fire_crt <- file.path(prefix, "fire")

fpa_out <- file.path(fire_crt, 'fpa_fod')
mtbs_out <- file.path(fire_crt, "mtbs_fod_perimeter_data")

s3_base <- 's3://earthlab-natem/ztrax-fire-interaction'
s3_results <- 's3://earthlab-natem/ztrax-fire-interaction/results'

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ecoregion_prefix, mtbs_prefix,
                anthro_out, fire_crt, ecoregion_out, mtbs_out, fpa_out)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))
