# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("usa_shp")){
  usa_shp <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                       dir = us_prefix,
                       layer = "cb_2016_us_state_20m",
                       outname = "usa") %>%
    filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
    st_transform(proj_ea) %>%  # e.g. US National Atlas Equal Area
    dplyr::select(STUSPS) %>%
    setNames(tolower(names(.)))
}

# Import the Level 1 Ecoregions
if (!exists('ecoregl1')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l1.gpkg'))) {
    
    # Download the Level 1 Ecoregions
    ecoregion_shp <- file.path(ecoregion_out, "NA_CEC_Eco_Level1.shp")
    if (!file.exists(ecoregion_shp)) {
      loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
      dest <- paste0(ecoregion_out, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = ecoregion_out)
      unlink(dest)
      assert_that(file.exists(ecoregion_shp))
    }
    
    ecoregl1 <- st_read(dsn = ecoregion_out, layer = "NA_CEC_Eco_Level1") %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
      st_make_valid() %>%
      st_intersection(., usa_shp) %>%
      mutate(region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                         "TROPICAL WET FORESTS",
                                                         "NORTHERN FORESTS"), "East",
                                        if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                                 "SOUTHERN SEMI-ARID HIGHLANDS",
                                                                 "TEMPERATE SIERRAS",
                                                                 "MEDITERRANEAN CALIFORNIA",
                                                                 "NORTHWESTERN FORESTED MOUNTAINS",
                                                                 "MARINE WEST COAST FOREST"), "West", "Central")))) %>%
      setNames(tolower(names(.)))
    
    st_write(ecoregl1, file.path(ecoregion_out, 'us_eco_l1.gpkg'), driver = 'GPKG')
    
    system(paste0("aws s3 sync ",
                  prefix, " ",
                  s3_base))
    
  } else {
    ecoregl1 <- sf::st_read(file.path(ecoregion_out, 'us_eco_l1.gpkg'))
  }
}

# Import the Level 3 Ecoregions
if (!exists('ecoreg')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l3.gpkg'))) {

  # Download the Level 3 Ecoregions
  ecoregion_shp <- file.path(ecoregion_out, "us_eco_l3.shp")
  if (!file.exists(ecoregion_shp)) {
    loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
    dest <- paste0(ecoregion_out, ".zip")
    download.file(loc, dest)
    unzip(dest, exdir = ecoregion_out)
    unlink(dest)
    assert_that(file.exists(ecoregion_shp))
  }

  ecoreg <- st_read(dsn = ecoregion_out, layer = "us_eco_l3", quiet= TRUE) %>%
    st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
    st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
    dplyr::select(US_L3CODE, US_L3NAME, NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME) %>%
    st_intersection(., usa_shp) %>%
    mutate(region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                       "TROPICAL WET FORESTS",
                                                       "NORTHERN FORESTS"), "East",
                                      if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                               "SOUTHERN SEMI-ARID HIGHLANDS",
                                                               "TEMPERATE SIERRAS",
                                                               "MEDITERRANEAN CALIFORNIA",
                                                               "NORTHWESTERN FORESTED MOUNTAINS",
                                                               "MARINE WEST COAST FOREST"), "West", "Central"))),
           regions = as.factor(if_else(region == "East" & stusps %in% c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", "TX", "OK"), "South East",
                                       if_else(region == "East" & stusps %in% c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "RI", "CT", "MI", "MD",
                                                                                "MA", "WI", "IL", "IN", "OH", "WV", "VA", "KY", "MO", "IA", "MN"), "North East",
                                               as.character(region))))) %>%
    setNames(tolower(names(.)))

  st_write(ecoreg, file.path(ecoregion_out, 'us_eco_l3.gpkg'), driver = 'GPKG')

  system(paste0("aws s3 sync ",
                prefix, " ",
                s3_base))

} else {
  ecoreg <- sf::st_read(file.path(ecoregion_out, 'us_eco_l3.gpkg'))
  }
}

#Clean and prep the MTBS data
if (!exists('mtbs_pts')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {

    # Download the MTBS fire polygons
    mtbs_shp <- file.path(mtbs_prefix, 'mtbs_fod_pts_20170501.shp')
    if (!file.exists(mtbs_shp)) {
      loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
      dest <- paste0(mtbs_prefix, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = mtbs_prefix)
      unlink(dest)
      assert_that(file.exists(mtbs_shp))
    }

    mtbs_pts <- st_read(dsn = mtbs_prefix,
                         layer = "mtbs_fod_pts_20170501", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%
      st_intersection(st_union(usa_shp)) %>%
      mutate(MTBS_ID = FIRE_ID,
             MTBS_FIRE_NAME = FIRENAME,
             MTBS_DISCOVERY_YEAR = FIRE_YEAR,
             MTBS_DISCOVERY_DAY = FIRE_DAY,
             MTBS_DISCOVERY_MONTH = FIRE_MON,
             MTBS_ACRES = R_ACRES,
             FIRE_BIDECADAL = ifelse(MTBS_DISCOVERY_YEAR > 1985 & MTBS_DISCOVERY_YEAR <= 1990, 1990,
                                       ifelse(MTBS_DISCOVERY_YEAR >= 1991 & MTBS_DISCOVERY_YEAR <= 1995, 1995,
                                               ifelse(MTBS_DISCOVERY_YEAR >= 1996 & MTBS_DISCOVERY_YEAR <= 2000, 2000,
                                                       ifelse(MTBS_DISCOVERY_YEAR >= 2001 & MTBS_DISCOVERY_YEAR <= 2005, 2005,
                                                               ifelse(MTBS_DISCOVERY_YEAR >= 2006 & MTBS_DISCOVERY_YEAR <= 2010, 2010,
                                                                       ifelse(MTBS_DISCOVERY_YEAR >= 2011 & MTBS_DISCOVERY_YEAR <= 2015, 2015,
                                                                               MTBS_DISCOVERY_YEAR))))))) %>%
      dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR, MTBS_ACRES, FIRE_BIDECADAL) %>%
      st_intersection(., ecoreg) %>%
      setNames(tolower(names(.)))

    st_write(mtbs_pts, file.path(mtbs_out, "mtbs_pts.gpkg"),
             driver = "GPKG")

    system(paste0("aws s3 sync ",
                  prefix, " ",
                  s3_base))
  } else {
    mtbs_pts <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg"))
  }
}

if (!exists('mtbs_fire')) {
  if (!file.exists(file.path(mtbs_out, "bidecadal_mtbs.gpkg"))) {

    #Download the MTBS fire polygons
    mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2', 'dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
    if (!file.exists(mtbs_shp)) {
      loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
      dest <- paste0(mtbs_prefix, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = mtbs_prefix)
      unlink(dest)
      assert_that(file.exists(mtbs_shp))
    }
    mtbs_pts_df <- as.data.frame(mtbs_pts) %>%
      dplyr::select(-geometry) %>%
      as_tibble()

    mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                         layer = "dissolve_mtbs_perims_1984-2015_DD_20170501", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%
      st_intersection(st_union(usa_shp)) %>%
      mutate(MTBS_ID = Fire_ID) %>%
      dplyr::select(MTBS_ID) %>%
      setNames(tolower(names(.))) %>%
      left_join(., mtbs_pts_df, by = c('mtbs_id')) %>%
      filter(fire_bidecadal > "1985") %>%
      na.omit()

    st_write(mtbs_fire, file.path(mtbs_out, "bidecadal_mtbs.gpkg"),
             driver = "GPKG")

    system(paste0("aws s3 sync ",
                  prefix, " ",
                  s3_base))
  } else {
    mtbs_fire <- st_read(file.path(mtbs_out, "bidecadal_mtbs.gpkg"))
  }
}

# Import the BUI data
bui_list <- list.files(file.path(anthro_out, 'built_up_intensity', 'BUI'),
                       pattern = "*.tif",
                       full.names = TRUE)

# setup parallel environment
sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c("mtbs_fire"))

extractions <- sfLapply(as.list(bui_list),
                        fun = extract_one,
                        shapefile_extractor = mtbs_fire)
sfStop()

# Import the FBUY data
if( !file.exists('data/anthro/first_year_built/FBUY/FBUY.csv')) {
  filename <- file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif')
  out_name <- gsub('.tif', '.csv', filename)
  
  fbuy_extractions <- raster(file.path(anthro_out, 'first_year_built', 'FBUY', 'FBUY.tif'))
  fbuy_extractions <- extract(fbuy_extractions, mtbs_fire, na.rm = TRUE, stat = 'sum', df = TRUE)
  write.csv(fbuy_extractions, file = out_name)
  
  system(paste0("aws s3 sync ", prefix, " ", s3_base)) 
  
} else {
  fbuy_extractions <- read_csv('data/anthro/first_year_built/FBUY/FBUY.csv')
}

if (!file.exists(file.path(mtbs_out, "mtbs_bui_5yr.gpkg"))) {
  # convert to a data frame
  extraction_df <- extractions %>%
    bind_cols %>%
    as_tibble %>%
    mutate(index = ID) %>%
    dplyr::select(-starts_with("ID")) %>%
    rename(ID = index) %>%
    group_by(ID) %>%
    summarise_all(funs(sum)) %>%
    ungroup() %>%
    mutate(mtbs_id = data.frame(mtbs_fire)$mtbs_id) %>%
    dplyr::select(-starts_with('X'), -ID) %>%
    left_join(mtbs_fire, ., by = 'mtbs_id') %>%
    mutate(BUI = ifelse(fire_bidecadal == "1990", BUI_1990,
                        ifelse(fire_bidecadal == "1995", BUI_1995,
                               ifelse(fire_bidecadal == "2000", BUI_2000,
                                      ifelse(fire_bidecadal == "2005", BUI_2005,
                                             ifelse(fire_bidecadal == "2010", BUI_2010,
                                                    ifelse(fire_bidecadal == "2015", BUI_2015,
                                                           0)))))))
  st_write(extraction_df, file.path(mtbs_out, "mtbs_bui_5yr.gpkg"))

  system(paste0("aws s3 sync ",
                prefix, " ",
                s3_base))
} else {

  extraction_df <- st_read(file.path(mtbs_out, "mtbs_bui_5yr.gpkg"))
}

