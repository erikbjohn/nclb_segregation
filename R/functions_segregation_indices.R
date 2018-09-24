functions_segregation_indices <- function(dt, map_year, geo_scale){
  # Dissimilarity and Exposure indices
  #
  # black to white Exposure Index
  # if exposure_bw=0.36 the probability of a Black student “interacting” with a White student is about
  # 36%. You can also interpret this to mean that 36 of every 100 students a Black student
  # meets will be White.
  # if exposure_wb=0.1 the probability of a White person “interacting” with a Black person is about
  # 10%. You can also interpret this to mean that 10 of every 100 people a White student
  # meets will be Black.
  dt <- dt[!is.na(BLACK)]
  dt <- dt[!is.na(WHITE)]
  dt <- dt[!is.na(geo_scale)]
  dt <- dt[, black_tot := sum(BLACK), by=geo_scale]
  dt <- dt[, white_tot := sum(WHITE), by=geo_scale]
  dt <- dt[, tot := black_tot + white_tot]
  
  # Dissimilarity Index
  dt <- dt[, school_share_diff := abs((BLACK/black_tot)-(WHITE/white_tot))]
  dt <- dt[, index_dissimilarity:=0.5*(sum(school_share_diff)), by=geo_scale]
  
  # Exposure BW
  dt <- dt[, school_exposure_bw:=(BLACK/black_tot)*(WHITE/tot)]
  dt <- dt[, index_exposure_bw:=sum(school_exposure_bw), by=geo_scale]
  
  # Exposure WB
  dt <- dt[, school_exposure_wb:=(WHITE/white_tot)*(BLACK/tot)]
  dt <- dt[, index_exposure_wb:=sum(school_exposure_wb), by=geo_scale]
  
  # Finnegan's measure
  ## I suggest we calculate a relative exposure index as (E-W)/W, 
  ## where E is the black-white exposure index and W is the percent of the district that is white.
  ## Thus, the index is interpretable as the relative distance from maximum contact
  ## or “the district is at a distance of X percent from a level of maximum (perfectly integrated) contact. 
  ## The white-black exposure is obviously (E-B)/B.
  ##
  ## This change will not only make the exposure index more interpretable, 
  ## but also allow us to compare different districts more easily. 
  ## Also, note that the new relative exposure index I propose is still a measure of 
  ## contact that is entirely separate from the dissimilarity index, which measures 
  ## how evenly two demographics are distributed throughout the district.
  
  dt <- dt[, share_white := white_tot/tot]
  dt <- dt[, share_black := black_tot/tot]
  
  dt <- dt[, index_finnegan := (index_exposure_bw)/share_white]

  dt$index_geo_scale <- geo_scale
  dt$index_geo_id <- dt[, geo_scale, with=FALSE]
  dt$index_geo_year <- map_year

  
  colnames <- c('NCESSCH', 'YEAR', 'index_geo_year', 'index_geo_scale', 'index_geo_id',
                'index_dissimilarity', 'index_exposure_bw', 'index_exposure_wb',
                'index_finnegan')
  dt <- unique(dt[, colnames, with = FALSE])
  
  
  return(dt)
}
