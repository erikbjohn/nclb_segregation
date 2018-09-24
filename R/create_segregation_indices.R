create_segregation_indices <- function(){
  create_segregation_indices_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/segregation_indices.rds'
  if(!(file.exists(create_segregation_indices_location))){
    # Year mapping
    l_district_maps <- get_district_maps()
    shp_county <- get_county_maps()
    l_schools <- get_schools()
    
    
    # Not all l_shps used in maps
    l_districts <- vector('list', length=25)
    l_districts[[1]] <- list(sYear='8990', numYear = 1989, map=l_district_maps[[1]])
    l_districts[[2]] <- list(sYear='8990', numYear = 1990, map=l_district_maps[[1]])
    l_districts[[3]] <- list(sYear='8990', numYear = 1991, map=l_district_maps[[1]])
    l_districts[[4]] <- list(sYear='8990', numYear = 1992, map=l_district_maps[[1]])
    l_districts[[5]] <- list(sYear='8990', numYear = 1993, map=l_district_maps[[1]])
    l_districts[[6]] <- list(sYear='8990', numYear = 1994, map=l_district_maps[[1]])
    
    l_districts[[7]] <- list(sYear='9596', numYear = 1995, map=l_district_maps[[1]])
    l_districts[[8]] <- list(sYear='9596', numYear = 1996, map=l_district_maps[[2]])
    
    l_districts[[9]] <- list(sYear='9798', numYear = 1997, map=l_district_maps[[3]])
    l_districts[[10]] <- list(sYear='9798', numYear = 1998, map=l_district_maps[[3]])

    l_districts[[11]] <- list(sYear='9900', numYear = 1999, map=l_district_maps[[4]])
    l_districts[[12]] <- list(sYear='9900', numYear = 2000, map=l_district_maps[[4]])
    
    l_districts[[13]] <- list(sYear='0102', numYear = 2001, map=l_district_maps[[5]])
    l_districts[[14]] <- list(sYear='0102', numYear = 2002, map=l_district_maps[[5]])
    
    l_districts[[15]] <- list(sYear='0304', numYear = 2003, map=l_district_maps[[6]])
    l_districts[[16]] <- list(sYear='0304', numYear = 2004, map=l_district_maps[[6]])
    
    l_districts[[17]] <- list(sYear='0506', numYear = 2006, map=l_district_maps[[7]])
    
    l_districts[[18]] <- list(sYear='0708', numYear = 2007, map=l_district_maps[[8]])
    l_districts[[19]] <- list(sYear='0708', numYear = 2008, map=l_district_maps[[8]])
    
    l_districts[[20]] <- list(sYear='0910', numYear = 2009, map=l_district_maps[[9]])
    l_districts[[21]] <- list(sYear='0910', numYear = 2010, map=l_district_maps[[9]])
    
    l_districts[[22]] <- list(sYear='1112', numYear = 2011, map=l_district_maps[[10]])
    l_districts[[23]] <- list(sYear='1112', numYear = 2012, map=l_district_maps[[10]])
    
    l_districts[[24]] <- list(sYear='1314', numYear = 2013, map=l_district_maps[[11]])
    l_districts[[25]] <- list(sYear='1314', numYear = 2014, map=l_district_maps[[11]])

    map_base_year <- l_districts[[1]]$map
    year_base_year <- l_districts[[1]]$numYear
    l_indices_years <- vector('list', length=length(l_districts))
    
    for(iYear in 1:length(l_districts)){
      l_indices <- vector(mode = 'list', length=3)
      l_map <- l_districts[[iYear]]
      map_year <- l_map$numYear
      cat(map_year, '\n')
      schools_pts_year <- l_schools$pts[l_schools$pts@data$YEAR == map_year,]
      schools_pts_year@data <- as.data.table(schools_pts_year@data)
      schools_pts_year@data <- schools_pts_year@data[, .(YEAR, BLACK, WHITE, NCESSCH)]
      
      # Indices based on 1990 school district boundaries
      over_base <- sp::over(schools_pts_year, map_base_year)
      over_base <- as.data.table(cbind(schools_pts_year@data, over_base))
      l_indices$base <- functions_segregation_indices(over_base, map_year = 'base_year', geo_scale='shp_LEA')
      
      # Indices based on contemporaneous district boundaries
      over_same <- sp::over(schools_pts_year, l_map$map)
      over_same <- as.data.table(cbind(schools_pts_year@data, over_same))
      l_indices$contemp <- functions_segregation_indices(over_same, map_year = 'contemp', geo_scale='shp_LEA')
        
      # Indices based on county boundaries
      over_counties <- over(schools_pts_year, shp_county)
      over_counties <- data.table(cbind(schools_pts_year@data, over_counties))
      l_indices$counties <- functions_segregation_indices(over_counties, map_year='2010', geo_scale='CNTYFIPS')
      
      l_indices_years[[iYear]] <- rbindlist(l_indices, use.names = TRUE, fill=TRUE)
    }   
    dt_indices <- rbindlist(l_indices_years, use.names = TRUE, fill=TRUE)
    saveRDS(dt_indices, file=create_segregation_indices_location)
  } else {
    dt_indices <- readRDS(create_segregation_indices_location)
  }
  return(dt_indices)
}