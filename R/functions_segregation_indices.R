functions_segregation_indices <- function(dt){
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
  dt <- dt[!is.na(shp_LEA)]
  dt <- dt[, black_tot := sum(BLACK), by=shp_LEA]
  dt <- dt[, white_tot := sum(WHITE), by=shp_LEA]
  dt <- dt[, tot := black_tot + white_tot]
  
  # Dissimilarity Index
  dt <- dt[, school_share_diff := abs((BLACK/black_tot)-(WHITE/white_tot))]
  dt <- dt[, dissimilarity:=0.5*(sum(school_share_diff)), by=shp_LEA]
#  dt <- dt[black_tot==0, dissimilarity:=1]
#  dt <- dt[white_tot==0, dissimilarity:=1]
  
  dt <- dt[, school_exposure_bw:=(BLACK/black_tot)*(WHITE/tot)]
  
  dt <- dt[, exposure_bw:=sum(school_exposure_bw), by=shp_LEA]
  dt <- dt[, school_exposure_wb:=(WHITE/white_tot)*(BLACK/tot)]
  dt <- dt[, exposure_wb:=sum(school_exposure_wb), by=shp_LEA]
  dt <- unique(dt[, .(shp_LEA, dissimilarity, exposure_bw, exposure_wb)])
  return(dt)
}