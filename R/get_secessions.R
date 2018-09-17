get_secessions <- function(){
  get_secessions_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/get_secessions.rds'
  library(sp)
  if(!file.exists(get_secessions_location)){
    # Schools locations
    schools <- readRDS('~/Dropbox/pkg.data/nclb_segregation/Clean/ejData_never_delete_me.rds')
    schools <- schools[, .(LEAID, NCESSCH, YEAR, BESTLON, BESTLAT)]
    schools <- schools[!(is.na(BESTLON))]
    schools <- schools[!(is.na(BESTLAT))]
    schools <- schools[BESTLAT>18]
    
    schools_unique <- unique(schools[, .(LEAID, NCESSCH, BESTLON=round(BESTLON,4), BESTLAT=round(BESTLAT,4))])
    coords <- cbind(Longitude = as.numeric(as.character(schools_unique$BESTLON)), 
                    Latitude = as.numeric(as.character(schools_unique$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_unique, proj4string = sp::CRS(proj_env))
    rgdal::writeOGR(schools_pts, '~/Downloads/', 'tmp_schools_pts', driver='ESRI Shapefile')
    schools_pts@data 
    
    # Define projection environment
    shp_07_08 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0708_tl08', 
                                stringsAsFactors = FALSE)
    shp_07_08@data <- data.table(shp_07_08@data)
    shp_07_08@data <- shp_07_08@data[!is.na(SCSDLEA), shp_LEA:=paste0(STATEFP, SCSDLEA)]
    shp_07_08@data <- shp_07_08@data[!is.na(ELSDLEA), shp_LEA:=paste0(STATEFP, ELSDLEA)]
    shp_07_08@data <- shp_07_08@data[!is.na(UNSDLEA), shp_LEA:=paste0(STATEFP, UNSDLEA)]
    proj_env <- proj4string(shp_07_08)
    
    # 1989/90
    shp_89_90 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy8990_tl95', 
                                stringsAsFactors = FALSE)
    shp_89_90@data <- data.table(shp_89_90@data)
    shp_89_90@data <- shp_89_90@data[, shp_LEA:=GEOID]
    proj4string(shp_89_90) <- proj_env
    
    
    # 1995/96
    shp_95_96 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy9596_tl97', 
                                stringsAsFactors = FALSE)
    shp_95_96@data <- data.table(shp_95_96@data)
    shp_95_96@data <- shp_95_96@data[, shp_LEA:=GEOID]
    proj4string(shp_95_96) <- proj_env
    
    # 1997/98
    shp_97_98 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy9798_tl98', 
                                stringsAsFactors = FALSE)
    shp_97_98@data <- data.table(shp_97_98@data)
    shp_97_98@data <- shp_97_98@data[, shp_LEA:=GEOID]
    proj4string(shp_97_98) <- proj_env
    
    # 1999/00)
    shp_99_00 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy9900_tl99', 
                                stringsAsFactors = FALSE)
    shp_99_00@data <- data.table(shp_99_00@data)
    shp_99_00@data <- shp_99_00@data[, shp_LEA:=GEOID]
    proj4string(shp_99_00) <- proj_env
    
    # 2001/02
    shp_01_02 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0102_tl02', 
                                stringsAsFactors = FALSE)
    shp_01_02@data <- data.table(shp_01_02@data)
    shp_01_02@data <- shp_01_02@data[, shp_LEA:=GEOID]
    proj4string(shp_01_02) <- proj_env
    
    # 2003/04
    shp_03_04 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0304_tl04', 
                                stringsAsFactors = FALSE)
    shp_03_04@data <- data.table(shp_03_04@data)
    shp_03_04@data <- shp_03_04@data[, shp_LEA:=GEOID]
    proj4string(shp_03_04) <- proj_env
    
    # 2005/06
    shp_05_06 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0506_tl06', 
                                stringsAsFactors = FALSE)
    shp_05_06@data <- data.table(shp_05_06@data)
    shp_05_06@data <- shp_05_06@data[, shp_LEA:=GEOID]
    proj4string(shp_05_06) <- proj_envrgdal::writeOGR(schools_pts, '~/Downloads/', 'tmp_schools_pts', driver='ESRI Shapefile')
    
    # 2007/08
    shp_07_08 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0708_tl08', 
                                stringsAsFactors = FALSE)
    shp_07_08@data <- data.table(shp_07_08@data)
    shp_07_08@data <- shp_07_08@data[, shp_LEA:=GEOID]
    proj4string(shp_07_08) <- proj_env
    
    # 2009/10
    shp_09_10 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy0910_tl10', 
                                stringsAsFactors = FALSE)
    shp_09_10@data <- data.table(shp_09_10@data)
    shp_09_10@data <- shp_09_10@data[, shp_LEA:=GEOID10]
    proj4string(shp_09_10) <- proj_env
    
    # 2011/12
    shp_11_12 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy1112_tl12', 
                                stringsAsFactors = FALSE)
    shp_11_12@data <- data.table(shp_11_12@data)
    shp_11_12@data <- shp_11_12@data[, shp_LEA:=GEOID]
    proj4string(shp_11_12) <- proj_env
    
    # 2013/14
    shp_13_14 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy1314_tl14', 
                                stringsAsFactors = FALSE)
    shp_13_14@data <- data.table(shp_13_14@data)
    shp_13_14@data <- shp_13_14@data[, shp_LEA:=GEOID]
    proj4string(shp_13_14) <- proj_env
    
    # 2015/16
    shp_15_16 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                                layer='schooldistrict_sy1516_tl16', 
                                stringsAsFactors = FALSE)
    shp_15_16@data <- data.table(shp_15_16@data)
    shp_15_16@data <- shp_15_16@data[, shp_LEA:=GEOID]
    proj4string(shp_15_16) <- proj_env
    
    # All shapes imports
    # shp_full <- readRDS('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/shp_districts_full.rds')
    
    # 1989/90 map is weird because school district ids (LEAID) change
    
    
    l_pts <- list()
    # Overlay schools on 1989/90 map
    dt_89_90 <- sp::over(schools_pts, shp_89_90)
    head(dt_89_90)
    head(schools_pts)
    dt_89_90 <- cbind(dt_89_90, schools_pts@data, year='8990')
    l_pts$dt_89_90 <- dt_89_90[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 1995/96 map
    dt_95_96 <- sp::over(schools_pts, shp_95_96)
    head(dt_95_96)
    head(schools_pts)
    dt_95_96 <- cbind(dt_95_96, schools_pts@data, year='9596')
    l_pts$dt_95_96 <- dt_95_96[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 1997/98 map
    dt_97_98 <- sp::over(schools_pts, shp_97_98)
    dt_97_98 <- cbind(dt_97_98, schools_pts@data, year='9798')
    l_pts$dt_97_98 <- dt_97_98[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 1999/00 map
    dt_99_00 <- sp::over(schools_pts, shp_99_00)
    dt_99_00 <- cbind(dt_99_00, schools_pts@data, year='9900')
    l_pts$dt_99_00 <- dt_99_00[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2001/02 map
    dt_01_02 <- sp::over(schools_pts, shp_01_02)
    dt_01_02 <- cbind(dt_01_02, schools_pts@data, year='0102')
    l_pts$dt_01_02 <- dt_01_02[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2003/04 map
    dt_03_04 <- sp::over(schools_pts, shp_03_04)
    dt_03_04 <- cbind(dt_03_04, schools_pts@data, year='0304')
    l_pts$dt_03_04 <- dt_03_04[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2005/06 map
    dt_05_06 <- sp::over(schools_pts, shp_05_06)
    dt_05_06 <- cbind(dt_05_06, schools_pts@data, year='0506')
    l_pts$dt_05_06 <- dt_05_06[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2007/08 map
    dt_07_08 <- sp::over(schools_pts, shp_07_08)
    dt_07_08 <- cbind(dt_07_08, schools_pts@data, year='0708')
    l_pts$dt_07_08 <- dt_07_08[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2009/10 map
    dt_09_10 <- sp::over(schools_pts, shp_09_10)
    dt_09_10 <- cbind(dt_09_10, schools_pts@data, year='0910')
    l_pts$dt_09_10 <- dt_09_10[, .(GEOID = GEOID10, NCESSCH, year)]
    
    # Overlay schools on 2011/12 map
    dt_11_12 <- sp::over(schools_pts, shp_11_12)
    dt_11_12 <- cbind(dt_11_12, schools_pts@data, year='1112')
    l_pts$dt_11_12 <- dt_11_12[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2013/2014  map
    dt_13_14 <- sp::over(schools_pts, shp_13_14)
    dt_13_14 <- cbind(dt_13_14, schools_pts@data, year='1314')
    l_pts$dt_13_14 <- dt_13_14[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2013/2014  map
    dt_15_16 <- sp::over(schools_pts, shp_15_16)
    dt_15_16 <- cbind(dt_15_16, schools_pts@data, year='1516')
    l_pts$dt_15_16 <- dt_15_16[, .(GEOID, NCESSCH, year)]
    
    dt_pts_dists <- rbindlist(l_pts, use.names = TRUE, fill = TRUE)
  
    # Start checking to identify which schools are in one school district for the whole time period
    l_status <- list()
    n_years <- length(unique(dt_pts_dists$year))
    dt_pts_dists <- dt_pts_dists[, n_location_years:=.N, by=.(NCESSCH, GEOID)]
    dt_analyze <- copy(dt_pts_dists)
    table(dt_pts_dists$n_location_years)
    # No change
    l_status$no_change <- unique(dt_pts_dists[n_location_years==n_years]$NCESSCH)
    dt_analyze <- copy(dt_analyze[!(NCESSCH %in% l_status$no_change)])
    
    
    
    # Year mapping
    l_maps <- vector('list', length=n_years)
    l_maps[[1]] <- list(sYear='8990', numYear = 1989, map=shp_89_90)
    l_maps[[2]] <- list(sYear='9596', numYear = 1995, map=shp_95_96)
    l_maps[[3]] <- list(sYear='9798', numYear = 1997, map=shp_97_98)
    l_maps[[4]] <- list(sYear='9900', numYear = 1999, map=shp_99_00)
    l_maps[[5]] <- list(sYear='0102', numYear = 2001, map=shp_01_02)
    l_maps[[6]] <- list(sYear='0304', numYear = 2003, map=shp_03_04)
    l_maps[[7]] <- list(sYear='0506', numYear = 2005, map=shp_05_06)
    l_maps[[8]] <- list(sYear='0708', numYear = 2007, map=shp_07_08)
    l_maps[[9]] <- list(sYear='0910', numYear = 2009, map=shp_09_10)
    l_maps[[10]] <- list(sYear='1112', numYear = 2011, map=shp_11_12)
    l_maps[[11]] <- list(sYear='1314', numYear = 2013, map=shp_13_14)
    l_maps[[12]] <- list(sYear='1516', numYear = 2015, map=shp_15_16)
  
    # For each school that has changed school district, find out year(s) and reasons
    school <- dt_analyze[NCESSCH=='10000600123']
    plot(shp_03_04[shp_03_04@data$GEOID=='0100006',], col=rgb(red=1, green=0, blue=0, alpha=0.3))
    plot(shp_15_16[shp_15_16@data$GEOID=='0100012',], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), add=TRUE)
    plot(shp_05_06[shp_05_06@data$GEOID=='0199016',], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.1), add=TRUE)
    plot(schools_pts[schools_pts@data$NCESSCH=='10000600123',], col = rgb(red = 0, green = 1, blue = 1, alpha = 1))
    # Is the new school disrict larger or smaller than the old one.
    
    
    
    
    ### Meeting with Amanda Segregation by county using base and new years
    schools <- schools[!(is.na(BESTLON))]
    schools <- schools[!(is.na(BESTLAT))]
    schools <- schools[BESTLAT>18]
    schools_unique <- unique(schools[, .(LEAID, YEAR, NCESSCH, BESTLON=round(BESTLON,4), BESTLAT=round(BESTLAT,4), BLACK, WHITE)])
    coords <- cbind(Longitude = as.numeric(as.character(schools_unique$BESTLON)), 
                    Latitude = as.numeric(as.character(schools_unique$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_unique, proj4string = sp::CRS(proj_env))
    
    map_base_year <- l_maps[[1]]$map
    for(iMap in 1:length(l_maps)){
      l_map <- l_maps[[iMap]]
      map_year <- l_map$numYear
      schools_pts_year <- schools_pts[schools_pts@data$YEAR == map_year,]
      schools_pts_year@data <- as.data.table(schools_pts_year@data)
      schools_pts_year@data <- schools_pts_year@data[, .(BLACK, WHITE, NCESSCH)]
      over_base <- sp::over(schools_pts_year, map_base_year)
      over_base <- as.data.table(cbind(schools_pts_year@data, over_base))
      over_same <- sp::over(schools_pts_year, l_map$map)
      over_same <- as.data.table(cbind(schools_pts_year@data, over_same))
      # Create Spatial Indices
      
    }   
    
    
    
}
