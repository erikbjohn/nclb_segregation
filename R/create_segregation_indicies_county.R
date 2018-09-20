create_segregation_indices_county <- function(){
  segregation_indices_county_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/segregation_indicies_county.rds'
  if(!file.exists(segregation_indices_county_location)){
    shp_county <- get_county_maps()
    l_schools <- get_schools()
    school_years <- l_schools$dt$YEAR
    for(school_year in school_years){
      pts_school_year <- l_schools$pts[l_schools$pts$YEAR %in% school_year,]
      schools_over_counties <- over(pts_school_year, shp_county)
      dt_schools_counties <- data.table(cbind(schools_over_counties, pts_school_year@data))
      setkey(dt_schools_counties, CNTYFIPS)
      seg_school_county <- functions_segregation_indices(dt_schools_counties,
                                                         geo_scale='CNTYFIPS')
      seg_school)
    }
    segregation_indices_county <- l_seg_county
    
    saveRDS(segregation_indices_county, file=segregation_indices_county_location)
  } else{
    segregation_indices_county <- readRDS(segregation_indices_county_location)
  }
  return(segregation_indices_county)
}