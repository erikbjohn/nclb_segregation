alabama_secessions <- function(){
  secessions <- get_secessions_import()
  secessions <- secessions[!is.na(secession_year) & !is.na(seceding_district_ncesid)]
  segregation <- create_segregation_indices()
  schools <- get_schools()
  districts <- get_district_maps()
  dt_secessions <- data.table()
  for(i_secession in 1:nrow(secessions)){
    cat(i_secession)
    l_outcomes <- list()
    secess_year <- secessions[i_secession]$secession_year
    secess_year_lag <- secess_year - 1
    secess_new_ncesid <- secessions[i_secession]$seceding_district_ncesid
    secess_from_ncesid <- secessions[i_secession]$seceded_from_district_ncesid
    secess_id <- secessions[i_secession]$secession_id
    
    l_outcomes$segs_from_before <- segregation[index_geo_id %in% secess_from_ncesid &
                                        YEAR %in% secess_year_lag &
                                        index_geo_year %in% 'contemp'][1]
    l_outcomes$segs_from_before$NCESSCH <- NULL
    l_outcomes$segs_from_before$LEAID_ID_boundary <- secess_from_ncesid
    l_outcomes$segs_from_before$type <- 'Pre-Secession Original District Segregation'
    
    l_outcomes$segs_from_after <- segregation[index_geo_id %in% secess_from_ncesid &
                                     YEAR %in% c(secess_year, secess_year +1) &
                                     index_geo_year %in% 'contemp'][1]
    l_outcomes$segs_from_after$NCESSCH <- NULL
    l_outcomes$segs_from_after$LEAID_ID_boundary <- secess_from_ncesid
    l_outcomes$segs_from_after$type <- 'Post-Secession Original District Segregation'
    
    
    l_outcomes$segs_new <- segregation[index_geo_id %in% secess_new_ncesid &
                              YEAR %in% c(secess_year, secess_year +(1:3)) &
                              index_geo_year %in% c('contemp', 'no_mapping_actual')][1]
    l_outcomes$segs_new$NCESSCH <- NULL
    l_outcomes$segs_new$LEAID_ID_boundary <- secess_new_ncesid
    l_outcomes$segs_new$type <- 'Post-Secession New District Segregation'
    
    
    # Map schools into
    before_year <- l_outcomes$segs_from_before$YEAR
    after_year <- l_outcomes$segs_new$YEAR
   
    dist_shp <- districts_17
    dist_shp <- dist_shp[dist_shp@data$STATEFP %in% '01',]
    dist_shp <- dist_shp[dist_shp@data$GEOID %in% secess_new_ncesid, ]
    
    pts_schools_before_year <- schools$pts[schools$pts@data$YEAR %in% before_year,]
    schools_before_year <- sp::over(pts_schools_before_year, dist_shp)
    schools_before_year <- data.table(cbind(schools_before_year, pts_schools_before_year@data))
    schools_before_year <- schools_before_year[!is.na(STATEFP)]
    l_outcomes$schools_before_year_segregation <- functions_segregation_indices(schools_before_year, before_year, 'LEAID')[1]
    l_outcomes$schools_before_year_segregation$NCESSCH <- NULL
    l_outcomes$schools_before_year_segregation$LEAID_ID_boundary <- secess_new_ncesid
    l_outcomes$schools_before_year_segregation$type <- 'Pre-Secession Segregation in Schools that Seceed later'
    
       
    # pts_schools_after_year <- schools$pts[schools$pts@data$YEAR %in% after_year,]
    # schools_after_year <- sp::over(pts_schools_after_year, dist_shp)
    # schools_after_year <- data.table(cbind(schools_after_year, pts_schools_after_year@data))
    # schools_after_year <- schools_after_year[!is.na(STATEFP)]
    # l_outcomes$schools_after_year_segregation <- functions_segregation_indices(schools_after_year, after_year, 'LEAID')[1]
    # l_outcomes$schools_after_year_segregation$NCESSCH <- NULL
    # l_outcomes$schools_after_year_segregation$LEAID_ID_boundary <- secess_new_ncesid
    # l_outcomes$schools_after_year_segregation$type <- 'Post-Secession Segregation in Schools that Seceed later'
    dt_secession <- data.table::rbindlist(l_outcomes, use.names = TRUE, fill=TRUE)
    dt_secession$secession_id <- secess_id
    dt_secessions <- rbindlist(list(dt_secessions, dt_secession), use.names=TRUE, fill=TRUE)
  }  

}
