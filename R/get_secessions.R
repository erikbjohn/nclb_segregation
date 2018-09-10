get_secessions <- function(){
  get_secessions_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/get_secessions.rds'
  library(sp)
  if(!file.exists(get_secessions_location)){
    # Schools locations
    schools <- readRDS('~/Box Sync/Segregation/SchoolData/public/ejData_never_delete_me.rds')
    schools <- schools[, .(LEAID, NCESSCH, YEAR, BESTLON, BESTLAT)]
    schools <- schools[!(is.na(BESTLON))]
    schools <- schools[!(is.na(BESTLAT))]
    schools <- schools[BESTLAT>18]
    
    # Define projection environment
    shp_07_08 <- rgdal::readOGR(dsn = path.expand('~/Box Sync/Segregation/SchoolData/district mapping/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0708_tl08', 
                                stringsAsFactors = FALSE)
    shp_07_08@data <- data.table(shp_07_08@data)
    shp_07_08@data <- shp_07_08@data[!is.na(SCSDLEA), LEAID:=paste0(STATEFP, SCSDLEA)]
    shp_07_08@data <- shp_07_08@data[!is.na(ELSDLEA), LEAID:=paste0(STATEFP, ELSDLEA)]
    shp_07_08@data <- shp_07_08@data[!is.na(UNSDLEA), LEAID:=paste0(STATEFP, UNSDLEA)]
    proj_env <- proj4string(shp_07_08)
    # Convert points (shapes)
    coords <- cbind(Longitude = as.numeric(as.character(schools$BESTLON)), 
                    Latitude = as.numeric(as.character(schools$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, dplyr::select(schools, -BESTLON, -BESTLAT), proj4string = sp::CRS(proj_env))
    schools_pts@data 
    # Try overlayiing on first map
    dt <- sp::over(schools_pts, shp_07_08)
    dt <- dt[, LEAID_MAP:=LEAID]
    schools_pts@data$LEADID_MAP08 <- dt$LEAID_MAP
    schools_pts@data <- schools_pts@data[, match_check:= FALSE][LEAID==LEADID_MAP08, match_check:=TRUE]
    schools[LEANM=='DULUTH PUBLIC SCHOOL DISTRICT' & SCHNAM=='EAST HIGH SCHOOL']
    schools_pts@data[grep('DULUTH', NAME)]
    schools_pts@data[schools_pts@data$LEAID %in% '2711040',]
  }
}
