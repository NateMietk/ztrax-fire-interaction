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

# Import the Level 3 Ecoregions
if (!exists('ecoreg')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l3.gpkg'))) {
  
  # Download the Level 3 Ecoregions
  ecoregion_shp <- file.path(ecoregion_out, "us_eco_l3.shp")
  if (!file.exists(ecoregion_shp)) {
    loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
    dest <- paste0(ecoregion_out, ".zip")
    download.file(loc, dest)
    unzip(dest, exdir = raw_prefix)
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

} else {
  ecoreg <- sf::st_read(file.path(ecoregion_out, 'us_eco_l3.gpkg'))
  }
}

#Clean and prep the MTBS data
if (!exists('mtbs_fire')) {
  if (!file.exists(file.path(mtbs_out, "mtbs_conus.gpkg"))) {
    
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
    
    mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                         layer = "dissolve_mtbs_perims_1984-2015_DD_20170501", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%
      mutate(MTBS_ID = Fire_ID,
             MTBS_FIRE_NAME = Fire_Name,
             MTBS_DISCOVERY_YEAR = Year,
             MTBS_DISCOVERY_DAY = StartDay,
             MTBS_DISCOVERY_MONTH = StartMonth,
             MTBS_ACRES = Acres,
             fire_bidecadal = ifelse(Year > 1985 & Year <= 1990, 1990,
                                       ifelse(Year >= 1991 & Year <= 1995, 1995,
                                               ifelse(Year >= 1996 & Year <= 2000, 2000,
                                                       ifelse(Year >= 2001 & Year <= 2005, 2005,
                                                               ifelse(Year >= 2006 & Year <= 2010, 2010,
                                                                       ifelse(Year >= 2011 & Year <= 2015, 2015, 
                                                                               Year))))))) %>%
      dplyr::select(MTBS_ID, MTBS_FIRE_NAME, MTBS_DISCOVERY_DAY, MTBS_DISCOVERY_MONTH, MTBS_DISCOVERY_YEAR, MTBS_ACRES, fire_bidecadal) %>%
      setNames(tolower(names(.)))
    
    st_write(mtbs_fire, file.path(mtbs_out, "mtbs_conus.gpkg"),
             driver = "GPKG")
    
  } else {
    mtbs_fire <- st_read(dsn = file.path(mtbs_out, "mtbs_conus.gpkg"))
  }
}

# Import the BUI data
bui_list <- list.files(file.path(anthro_out, 'built_up_intensity', 'BUI'),
                       pattern = "*.tif",
                       full.names = TRUE)
bui <- stack(bui_list)
#https://gis.stackexchange.com/questions/246360/how-parallelize-the-extract-function-for-raster-files-in-r

cells <- cellnumbers(stack.ts[[1]], ts.poly) 
exvalues <- lapply(seq_len(nlayers(stack.ts)), function(i) extract(readAll(stack.ts[[i]]), cells$cell_))


test.df %>% mutate(G1 = ifelse(Group == "G1", NA, G1))