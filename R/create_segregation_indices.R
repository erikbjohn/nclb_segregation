create_segregation_indices <- function(){
  create_segregation_indices_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/segregation_indices.rds'
  if(!(file.exists(create_segregation_indices_location))){
    # Year mapping
    l_district_maps <- get_district_maps()
    l_schools <- get_schools()

    # Not all l_shps used in maps
    l_districts <- vector('list', length=11)
    l_districts[[1]] <- list(sYear='8990', numYear = 1990, map=l_district_maps[[1]])
    l_districts[[2]] <- list(sYear='9596', numYear = 1996, map=l_district_maps[[2]])
    l_districts[[3]] <- list(sYear='9798', numYear = 1998, map=l_district_maps[[3]])
    l_districts[[4]] <- list(sYear='9900', numYear = 2000, map=l_district_maps[[4]])
    l_districts[[5]] <- list(sYear='0102', numYear = 2002, map=l_district_maps[[5]])
    l_districts[[6]] <- list(sYear='0304', numYear = 2004, map=l_district_maps[[6]])
    l_districts[[7]] <- list(sYear='0506', numYear = 2006, map=l_district_maps[[7]])
    l_districts[[8]] <- list(sYear='0708', numYear = 2008, map=l_district_maps[[8]])
    l_districts[[9]] <- list(sYear='0910', numYear = 2010, map=l_district_maps[[9]])
    l_districts[[10]] <- list(sYear='1112', numYear = 2012, map=l_district_maps[[10]])
    l_districts[[11]] <- list(sYear='1314', numYear = 2014, map=l_district_maps[[11]])

    map_base_year <- l_districts[[1]]$map
    year_base_year <- l_districts[[1]]$numYear
    l_indices_years <- vector('list', length=length(l_districts))
    
    for(iYear in 1:length(l_districts)){
      l_map <- l_districts[[iYear]]
      map_year <- l_map$numYear
      cat(map_year, '\n')
      schools_pts_year <- l_schools$pts[l_schools$pts@data$YEAR == map_year,]
      schools_pts_year@data <- as.data.table(schools_pts_year@data)
      schools_pts_year@data <- schools_pts_year@data[, .(BLACK, WHITE, NCESSCH)]
      over_base <- sp::over(schools_pts_year, map_base_year)
      over_base <- as.data.table(cbind(schools_pts_year@data, over_base))
      over_same <- sp::over(schools_pts_year, l_map$map)
      over_same <- as.data.table(cbind(schools_pts_year@data, over_same))
      indices_base <- functions_segregation_indices(over_base, geo_scale='shp_LEA')
      indices_same <- functions_segregation_indices(over_same, geo_scale='shp_LEA')
      l_indices_years[[iYear]] <- list(year=map_year,
                                year_base = year_base_year,
                                dt_indices_base = indices_base,
                                dt_indices_same = indices_same)
    }   
    for(iInd in 1:length(l_indices_years)){
      l_indices_years[[iInd]]$dt_indices_base$year <- l_indices_years[[iInd]]$year
      l_indices_years[[iInd]]$dt_indices_base$year_base <- l_indices_years[[iInd]]$year_base
      l_indices_years[[iInd]]$dt_indices_same$year <- l_indices_years[[iInd]]$year
    }
    saveRDS(l_indices_years, file=create_segregation_indices_location)
  } else {
    l_indices_years <- readRDS(create_segregation_indices_location)
  }
  return(l_indices_years)
}