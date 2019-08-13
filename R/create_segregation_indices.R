create_segregation_indices <- function(){
  create_segregation_indices_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/segregation_indices.rds'
  if(!(file.exists(create_segregation_indices_location))){
    # Year mapping
    l_districts <- get_district_maps()
    shp_county <- get_county_maps()
    l_schools <- get_schools()  
    
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
      
      # Indices based on 'actual' (no forced mapping of charter schools)
      actual <- l_schools$dt[YEAR==map_year, .(YEAR, BLACK, WHITE, NCESSCH, GEOID=LEAID, STATEFP=stringr::str_sub(LEAID, 1, 2))]
      l_indices$actual <- functions_segregation_indices(actual, map_year='no_mapping_acutal', geo_scale='GEOID')
      
      l_indices_years[[iYear]] <- rbindlist(l_indices, use.names = TRUE, fill=TRUE)
    }   
    dt_indices <- rbindlist(l_indices_years, use.names = TRUE, fill=TRUE)
    saveRDS(dt_indices, file=create_segregation_indices_location)
  } else {
    dt_indices <- readRDS(create_segregation_indices_location)
  }
  return(dt_indices)
}