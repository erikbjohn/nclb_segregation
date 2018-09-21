create_segregation_indices_county <- function(){
  segregation_indices_county_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/segregation_indicies_county.rds'
  if(!file.exists(segregation_indices_county_location)){
    shp_county <- get_county_maps()
    l_schools <- get_schools()
    school_years <- as.character(sort(as.numeric(unique(l_schools$dt$YEAR))))
    seg_school_counties <- data.table()
    for(school_year in school_years){
      cat(school_year)
      pts_school_year <- l_schools$pts[l_schools$pts$YEAR %in% school_year,]
      schools_over_counties <- over(pts_school_year, shp_county)
      dt_schools_counties <- data.table(cbind(schools_over_counties, pts_school_year@data))
      setkey(dt_schools_counties, CNTYFIPS)
      seg_school_county <- functions_segregation_indices(dt_schools_counties,
                                                         geo_scale='CNTYFIPS')
      seg_school_counties <- rbindlist(list(seg_school_counties, seg_school_county), use.names = TRUE, fill=TRUE)
    }
    seg_school_counties <- seg_school_counties[!is.na(CNTYFIPS)]
    
    segregation_indices_county <- seg_school_counties
    
    saveRDS(segregation_indices_county, file=segregation_indices_county_location)
  } else{
    segregation_indices_county <- readRDS(segregation_indices_county_location)
  }
  return(segregation_indices_county)
}
