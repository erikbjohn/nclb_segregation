create_segregation_indices <- function(){
  
  segregation_indices_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/segregation_indices.rds'
  if(!(file.exists(segregation_indices_location))){
    # Year mapping
    l_shps <- get_district_maps()
    
    l_maps <- vector('list', length=n_years)
    l_maps[[1]] <- list(sYear='8990', numYear = 1990, map=l_shps$shp_89_90)
    l_maps[[2]] <- list(sYear='9596', numYear = 1996, map=l_shps$shp_95_96)
    l_maps[[3]] <- list(sYear='9798', numYear = 1998, map=l_shps$shp_97_98)
    l_maps[[4]] <- list(sYear='9900', numYear = 2000, map=l_shps$shp_99_00)
    l_maps[[5]] <- list(sYear='0102', numYear = 2003, map=l_shps$shp_01_02)
    l_maps[[6]] <- list(sYear='0304', numYear = 2004, map=l_shps$shp_03_04)
    l_maps[[7]] <- list(sYear='0506', numYear = 2006, map=l_shps$shp_05_06)
    l_maps[[8]] <- list(sYear='0708', numYear = 2008, map=l_shps$shp_07_08)
    l_maps[[9]] <- list(sYear='0910', numYear = 2010, map=l_shps$shp_09_10)
    l_maps[[10]] <- list(sYear='1112', numYear = 2012, map=l_shps$shp_11_12)
    l_maps[[11]] <- list(sYear='1314', numYear = 2014, map=l_shps$shp_13_14)

    # Is the new school disrict larger or smaller than the old one.
    ### Meeting with Amanda Segregation by county using base and new years
    
    schools <- schools[!(is.na(BESTLON))]
    schools <- schools[!(is.na(BESTLAT))]
    schools <- schools[BESTLAT>18]
    schools_unique <- unique(schools[, .(LEAID, YEAR, NCESSCH, BESTLON=round(BESTLON,4), BESTLAT=round(BESTLAT,4), BLACK, WHITE)])
    coords <- cbind(Longitude = as.numeric(as.character(schools_unique$BESTLON)), 
                    Latitude = as.numeric(as.character(schools_unique$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_unique, proj4string = sp::CRS(proj_env))
    
    l_maps <- l_maps[which(sapply(l_maps, function(x) !is.null(x)))]
    map_base_year <- l_maps[[1]]$map
    year_base_year <- l_maps[[1]]$numYear
    l_indices <- vector('list', length=length(l_maps))
    
    for(iMap in 1:length(l_maps)){
      l_map <- l_maps[[iMap]]
      map_year <- l_map$numYear
      cat(map_year, '\n')
      schools_pts_year <- schools_pts[schools_pts@data$YEAR == map_year,]
      schools_pts_year@data <- as.data.table(schools_pts_year@data)
      schools_pts_year@data <- schools_pts_year@data[, .(BLACK, WHITE, NCESSCH)]
      over_base <- sp::over(schools_pts_year, map_base_year)
      over_base <- as.data.table(cbind(schools_pts_year@data, over_base))
      over_same <- sp::over(schools_pts_year, l_map$map)
      over_same <- as.data.table(cbind(schools_pts_year@data, over_same))
      indices_base <- functions_segregation_indices(over_base)
      indices_same <- functions_segregation_indices(over_same)
      l_indices[[iMap]] <- list(year=map_year,
                                year_base = year_base_year,
                                dt_indices_base = indices_base,
                                dt_indices_same = indices_same)
    }   
    saveRDS(l_indices, file=segregation_indices_location)
  } else {
    l_indices <- readRDS(segregation_indices_location)
  }
  return(l_indices)
}