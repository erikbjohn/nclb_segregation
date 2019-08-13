get_district_maps <- function(){
  # https://nces.ed.gov/programs/edge/Geographic/DistrictBoundaries
  district_maps_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/district_maps.rds'
  if(!file.exists(district_maps_location)){
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
  shp_89_90@data <- data.table(shp_89_90@data, YEAR=1990)
  shp_89_90@data <- shp_89_90@data[, shp_LEA:=GEOID]
  proj4string(shp_89_90) <- proj_env
  
  
  # 1995/96
  shp_95_96 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy9596_tl97', 
                              stringsAsFactors = FALSE)
  shp_95_96@data <- data.table(shp_95_96@data, YEAR=1996)
  shp_95_96@data <- shp_95_96@data[, shp_LEA:=GEOID]
  proj4string(shp_95_96) <- proj_env
  
  # 1997/98
  shp_97_98 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy9798_tl98', 
                              stringsAsFactors = FALSE)
  shp_97_98@data <- data.table(shp_97_98@data, YEAR=1998)
  shp_97_98@data <- shp_97_98@data[, shp_LEA:=GEOID]
  proj4string(shp_97_98) <- proj_env
  
  # 1999/00)
  shp_99_00 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy9900_tl99', 
                              stringsAsFactors = FALSE)
  shp_99_00@data <- data.table(shp_99_00@data, YEAR=2000)
  shp_99_00@data <- shp_99_00@data[, shp_LEA:=GEOID]
  proj4string(shp_99_00) <- proj_env
  
  # 2001/02
  shp_01_02 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy0102_tl02', 
                              stringsAsFactors = FALSE)
  shp_01_02@data <- data.table(shp_01_02@data, YEAR=2002)
  shp_01_02@data <- shp_01_02@data[, shp_LEA:=GEOID]
  proj4string(shp_01_02) <- proj_env
  
  # 2003/04
  shp_03_04 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy0304_tl04', 
                              stringsAsFactors = FALSE)
  shp_03_04@data <- data.table(shp_03_04@data, YEAR=2004)
  shp_03_04@data <- shp_03_04@data[, shp_LEA:=GEOID]
  proj4string(shp_03_04) <- proj_env
  
  # 2005/06
  shp_05_06 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy0506_tl06', 
                              stringsAsFactors = FALSE)
  shp_05_06@data <- data.table(shp_05_06@data, YEAR=2006)
  shp_05_06@data <- shp_05_06@data[, shp_LEA:=GEOID]
  proj4string(shp_05_06) <- proj_env
  
  # 2007/08
  shp_07_08 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy0708_tl08', 
                              stringsAsFactors = FALSE)
  shp_07_08@data <- data.table(shp_07_08@data, YEAR=2008)
  shp_07_08@data <- shp_07_08@data[, shp_LEA:=GEOID]
  proj4string(shp_07_08) <- proj_env
  
  # 2009/10
  shp_09_10 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy0910_tl10', 
                              stringsAsFactors = FALSE)
  shp_09_10@data <- data.table(shp_09_10@data, YEAR=2010)
  shp_09_10@data <- shp_09_10@data[, shp_LEA:=GEOID10]
  proj4string(shp_09_10) <- proj_env
  
  # 2011/12
  shp_11_12 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy1112_tl12', 
                              stringsAsFactors = FALSE)
  shp_11_12@data <- data.table(shp_11_12@data, YEAR=2012)
  shp_11_12@data <- shp_11_12@data[, shp_LEA:=GEOID]
  proj4string(shp_11_12) <- proj_env
  
  # 2013/14
  shp_13_14 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy1314_tl14', 
                              stringsAsFactors = FALSE)
  shp_13_14@data <- data.table(shp_13_14@data, YEAR=2014)
  shp_13_14@data <- shp_13_14@data[, shp_LEA:=GEOID]
  proj4string(shp_13_14) <- proj_env
  
  # 2015/16
  shp_15_16 <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/School_distrct_mapping/'),
                              layer='schooldistrict_sy1516_tl16', 
                              stringsAsFactors = FALSE)
  shp_15_16@data <- data.table(shp_15_16@data, YEAR=2016)
  shp_15_16@data <- shp_15_16@data[, shp_LEA:=GEOID]
  proj4string(shp_15_16) <- proj_env
  
  
  
  l_district_maps <- list(shp_89_90, shp_95_96, shp_97_98,
                          shp_99_00, shp_01_02, shp_03_04,
                          shp_05_06, shp_07_08, shp_09_10, 
                          shp_11_12, shp_13_14, shp_15_16)
  
  l_districts <- vector('list', length=25)
  l_districts[[1]] <- list(sYear='8990', numYear = 1989, map=l_district_maps[[1]], iMap = 1)
  l_districts[[2]] <- list(sYear='8990', numYear = 1990, map=l_district_maps[[1]], iMap = 1)
  l_districts[[3]] <- list(sYear='8990', numYear = 1991, map=l_district_maps[[1]], iMap = 1)
  l_districts[[4]] <- list(sYear='8990', numYear = 1992, map=l_district_maps[[1]], iMap = 1)
  l_districts[[5]] <- list(sYear='8990', numYear = 1993, map=l_district_maps[[1]], iMap = 1)
  l_districts[[6]] <- list(sYear='8990', numYear = 1994, map=l_district_maps[[1]], iMap = 1)
  
  l_districts[[7]] <- list(sYear='9596', numYear = 1995, map=l_district_maps[[1]], iMap = 1)
  l_districts[[8]] <- list(sYear='9596', numYear = 1996, map=l_district_maps[[2]], iMap = 2)
  
  l_districts[[9]] <- list(sYear='9798', numYear = 1997, map=l_district_maps[[3]], iMap = 3)
  l_districts[[10]] <- list(sYear='9798', numYear = 1998, map=l_district_maps[[3]], iMap = 3)
  
  l_districts[[11]] <- list(sYear='9900', numYear = 1999, map=l_district_maps[[4]], iMap = 4)
  l_districts[[12]] <- list(sYear='9900', numYear = 2000, map=l_district_maps[[4]], iMap = 4)
  
  l_districts[[13]] <- list(sYear='0102', numYear = 2001, map=l_district_maps[[5]], iMap = 5)
  l_districts[[14]] <- list(sYear='0102', numYear = 2002, map=l_district_maps[[5]], iMap = 5)
  
  l_districts[[15]] <- list(sYear='0304', numYear = 2003, map=l_district_maps[[6]], iMap = 6)
  l_districts[[16]] <- list(sYear='0304', numYear = 2004, map=l_district_maps[[6]], iMap = 6)
  
  l_districts[[17]] <- list(sYear='0506', numYear = 2006, map=l_district_maps[[7]], iMap = 7)
  
  l_districts[[18]] <- list(sYear='0708', numYear = 2007, map=l_district_maps[[8]], iMap = 8)
  l_districts[[19]] <- list(sYear='0708', numYear = 2008, map=l_district_maps[[8]], iMap = 8)
  
  l_districts[[20]] <- list(sYear='0910', numYear = 2009, map=l_district_maps[[9]], iMap = 9)
  l_districts[[21]] <- list(sYear='0910', numYear = 2010, map=l_district_maps[[9]], iMap = 9)
  
  l_districts[[22]] <- list(sYear='1112', numYear = 2011, map=l_district_maps[[10]], iMap = 10)
  l_districts[[23]] <- list(sYear='1112', numYear = 2012, map=l_district_maps[[10]], iMap = 10)
  
  l_districts[[24]] <- list(sYear='1314', numYear = 2013, map=l_district_maps[[11]], iMap = 11)
  l_districts[[25]] <- list(sYear='1314', numYear = 2014, map=l_district_maps[[11]], iMap = 11)
  
  l_districts[[26]] <- list(sYear='1516', numYear = 2015, map=l_district_maps[[12]], iMap = 12)
  l_districts[[27]] <- list(sYear='1516', numYear = 2016, map=l_district_maps[[12]], iMap = 12)
  l_districts[[28]] <- list(sYear='1516', numYear = 2017, map=l_district_maps[[12]], iMap = 12)
  
  names(l_districts) <- sapply(l_districts, '[[', 2)
  saveRDS(l_districts, file=district_maps_location)
   } else {
    district_maps <- readRDS(district_maps_location)
  }
  return(district_maps)
}