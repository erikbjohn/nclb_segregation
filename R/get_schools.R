get_schools <- function(){
  schools_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/schools.rds'
  
  ### NEED 2015-2018 HERE:https://nces.ed.gov/ccd/pubschuniv.asp
  
  if(!file.exists(schools_location)){
    # Schools locations
    proj_env <- get_proj_env()
    schools <- readRDS('~/Dropbox/pkg.data/nclb_segregation/Clean/ejData_never_delete_me.rds')
    source('R/get_schools_post2014.R')
    schools_post2014 <- get_schools_post2014()
    
    # TRY AND bind the schools and schools_post2014 data
    schools$IFTE <- NULL
    schools$IMEMB <- NULL
    
    #names_old <- names(schools)
    #names_post14 <- names(schools_post2014)
    #names_old[!(names_old %in% names_post14)]
    #names_post14[!(names_post14 %in% names_old)]
    
    # Remove multiple records for 2006
    setkey(schools, YEAR, LEAID, NCESSCH)
    # Delete duplicate records
    schools <- schools[, n:=1:.N, by=key(schools)]
    schools <- schools[n==1]
    schools$n <- NULL
    schools <- schools[!(is.na(BESTLON))]
    schools <- schools[!(is.na(BESTLAT))]
    schools <- schools[BESTLAT>18]
    schools <- schools[, NCESSCH := as.character(NCESSCH)]
    
    # Join to post 14 school data
    schools <- rbindlist(list(schools, schools_post2014), use.names = TRUE, fill=TRUE)
    
    # Data check on black white
    # Clean data
  
    # White = 0 and Black = 0 (turn to NA)
    table(stringr::str_sub(schools[BLACK==0 & WHITE==0]$LEAID, 1, 2), schools[BLACK==0 & WHITE==0]$YEAR)
    schools <- schools[BLACK <= 0 & WHITE <= 0, BLACK:=NA]
    schools <- schools[BLACK <= 0 & WHITE <= 0, WHITE:=NA]
    schools <- schools[BLACK < 0, BLACK:=NA]
    schools <- schools[WHITE < 0 , WHITE:=NA]
    
    # Black is na, white is not.
    table(stringr::str_sub(schools[is.na(BLACK) & !is.na(WHITE)]$LEAID, 1, 2), schools[is.na(BLACK) & !is.na(WHITE)]$YEAR)
    setkey(schools, NCESSCH, YEAR)
    iter <- 0
    na_black_num <-1
    while(iter < 12 & na_black_num > 0){
      iter <- iter + 1
      cat(iter, '\n')
      schools <- schools[, lead := shift(BLACK, iter, type='lead'), by=NCESSCH]
      schools <- schools[is.na(BLACK) & !is.na(WHITE), BLACK:=lead]
      schools <- schools[, lag := shift(BLACK, iter, type='lag'), by=NCESSCH]
      schools <- schools[is.na(BLACK) & !is.na(WHITE), BLACK:=lag]
      na_black_num <- nrow(schools[is.na(BLACK) & !is.na(WHITE)])
      cat(na_black_num, '\n')
    }
    schools[NCESSCH=='10000601434',.(BLACK, WHITE, YEAR)]
    #table(stringr::str_sub(schools[is.na(BLACK) & !is.na(WHITE)]$LEAID, 1, 2), schools[is.na(BLACK) & !is.na(WHITE)]$YEAR)
    ncessh_drop <- unique(schools[is.na(BLACK) & !is.na(WHITE)]$NCESSCH)
    schools <- schools[!(NCESSCH %in% ncessh_drop)]
    
    # Drop White is na, black is not (mental health stuff)
    schools_to_drop <- unique(schools[!is.na(BLACK) & is.na(WHITE)]$NCESSCH)
    schools <- schools[!(NCESSCH %in% schools_to_drop)]
    schools[NCESSCH=='10000601434',.(BLACK, WHITE, YEAR)]
    
    # Black and white are both NA
    na_num <-1
    iter <- 0
    while(iter < 12 & na_num > 0){
      iter <- iter + 1
      cat(iter, '\n')
      # Fix BLACK
      schools <- schools[, `:=`(lead_black = shift(BLACK, iter, type='lead'),
                               lead_white = shift(WHITE, iter, type='lead')),
                         by=NCESSCH]
      schools <- schools[is.na(BLACK) & is.na(WHITE), `:=`(BLACK=lead_black, WHITE=lead_white)]
      
      schools <- schools[, `:=`(lag_black = shift(BLACK, iter, type='lag'),
                                lag_white = shift(WHITE, iter, type='lag')),
                         by=NCESSCH]
      schools <- schools[is.na(BLACK) & is.na(WHITE), `:=`(BLACK=lag_black, WHITE=lag_white)]
      
      na_num <- nrow(schools[is.na(BLACK) & is.na(WHITE)])
      cat(na_num, '\n')
    }
    schools[NCESSCH=='10000601434',.(BLACK, WHITE, YEAR)]
    ncessh_drop <- unique(schools[is.na(BLACK) & is.na(WHITE)]$NCESSCH)
    schools <- schools[!(NCESSCH %in% ncessh_drop)]
    
    # Double check for weird records (assigned 0 for white when not true)
    schools <- schools[, white_min := min(WHITE), by=NCESSCH]
    schools <- schools[, white_max := max(WHITE), by=NCESSCH]
    schools <- schools[, white_diff := white_max - white_min]
    schools <- schools[, black_min := min(BLACK), by=NCESSCH]
    schools <- schools[, black_max := max(BLACK), by=NCESSCH]
    schools <- schools[, black_diff := black_max - black_min]
    
    # Drop virtual/cyber schools    saveRDS(l_schools
    LEANMs <- as.character(unique(schools$LEANM))
    LEANMs <- LEANMs[stringr::str_detect(LEANMs, '(?i)(cyber|virtual|electronic )')]
    schools <- schools[, LEANM:=as.character(LEANM)]
    schools <- schools[!(LEANM %in% LEANMs)]
    schools[NCESSCH=='10000601434',.(BLACK, WHITE, YEAR)]
    
    hist(schools[white_min==0 & white_max>5]$white_max, breaks=100)
    na_num <-1
    iter <- 0
    while(iter < 12 & na_num > 0){
      iter <- iter + 1
      cat(iter, '\n')
      schools <- schools[, lead := shift(WHITE, iter, type='lead'), by=NCESSCH]
      schools <- schools[white_min==0 & white_max > 10 & WHITE==0, WHITE:=lead]
      schools <- schools[, lag := shift(WHITE, iter, type='lag'), by=NCESSCH]
      schools <- schools[white_min==0 & white_max> 10 & WHITE==0, WHITE:=lag]
      na_num <- nrow(schools[white_min==0 & white_max>10 & WHITE==0])
      cat(na_num, '\n')
    }
    schools[NCESSCH=='10000601434',.(BLACK, WHITE, YEAR)]
    schools[NCESSCH=='174080004097',.(BLACK, WHITE, YEAR)]
    hist(schools[black_min==0 & black_max>5]$black_max, breaks=100)
    na_num <-1
    iter <- 0
    while(iter < 12 & na_num > 0){
      iter <- iter + 1
      cat(iter, '\n')
      schools <- schools[, lead := shift(BLACK, iter, type='lead'), by=NCESSCH]
      schools <- schools[black_min==0 & black_max > 10 & BLACK==0, BLACK:=lead]
      schools <- schools[, lag := shift(BLACK, iter, type='lag'), by=NCESSCH]
      schools <- schools[black_min==0 & black_max> 10 & BLACK==0, BLACK:=lag]
      na_num <- nrow(schools[black_min==0 & black_max>10 & BLACK==0])
      cat(na_num, '\n')
    }
    schools[NCESSCH=='10000601434',.(BLACK, WHITE, YEAR)]
    schools[NCESSCH=='174080004097',.(BLACK, WHITE, YEAR)]
    
    # Find outliers
    schools <- schools[, `:=`(white_mean=mean(WHITE),
                              white_sd=sd(WHITE),
                              black_mean=mean(BLACK),
                              black_sd=sd(BLACK)), by=NCESSCH][,`:=`(
                                white_low=pmax(0, white_mean-3.5*(white_sd)),
                                white_high=white_mean+4*(white_sd),
                                black_low=pmax(0, black_mean-3.5*(black_sd)),
                                black_high=black_mean+4*(black_sd))]
    
    schools[(BLACK < black_low | BLACK > black_high) & black_mean > 50]$NCESSCH
    schools[(WHITE < white_low | WHITE > white_high) & white_mean > 50]$NCESSCH
    
    # Clean these up (input errors)
    schools <- schools[, `:=`(black_lag=shift(BLACK,1, type='lag'),
                              black_lead=shift(BLACK, 1, type='lead'),
                              white_lag=shift(WHITE, 1, type='lag'),
                              white_lead=shift(WHITE, 1, type='lead'))]
    
    nrow(schools[(BLACK < black_low | BLACK > black_high) & black_mean > 50])
    schools <- schools[(BLACK < black_low | BLACK > black_high) & black_mean > 50, BLACK:=black_lag]
    nrow(schools[(BLACK < black_low | BLACK > black_high) & black_mean > 50])
    schools <- schools[(BLACK < black_low | BLACK > black_high) & black_mean > 50, BLACK:=black_lead]
    nrow(schools[(BLACK < black_low | BLACK > black_high) & black_mean > 50])
    
    nrow(schools[(WHITE < white_low | WHITE > white_high) & white_mean > 50])
    schools <- schools[(WHITE < white_low | WHITE > white_high) & white_mean > 50, WHITE:=white_lag]
    nrow(schools[(WHITE < white_low | WHITE > white_high) & white_mean > 50])
    schools <- schools[(WHITE < white_low | WHITE > white_high) & white_mean > 50, WHITE:=white_lead]
    nrow(schools[(WHITE < white_low | WHITE > white_high) & white_mean > 50])
    
    # Clean up all post 2014 stuff such as lifted and georeferencing
    
    ## Clean up georeferencing for 2015 and 2016 years
    na_num <-1
    iter <- 0
    setkey(schools, NCESSCH, YEAR)
    while(iter < 4 & na_num > 0){
      iter <- iter + 1
      cat(iter, '\n')
      schools <- schools[, lagLat := shift(BESTLAT, iter, type='lag'), by=NCESSCH]
      schools <- schools[, lagLon := shift(BESTLON, iter, type='lag'), by=NCESSCH]
      schools <- schools[is.na(BESTLAT), BESTLAT:=lagLat]
      schools <- schools[is.na(BESTLON), BESTLON:=lagLon]
      na_num <- nrow(schools[is.na(BESTLAT) | is.na(BESTLON)])
      cat(na_num, '\n')
    }
    
    na_num <-1
    iter <- 0
    setkey(schools, NCESSCH, YEAR)
    while(iter < 4 & na_num > 0){
      iter <- iter + 1
      cat(iter, '\n')
      schools <- schools[, leadLat := shift(BESTLAT, iter, type='lead'), by=NCESSCH]
      schools <- schools[, leadLon := shift(BESTLON, iter, type='lead'), by=NCESSCH]
      schools <- schools[is.na(BESTLAT), BESTLAT:=leadLat]
      schools <- schools[is.na(BESTLON), BESTLON:=leadLon]
      na_num <- nrow(schools[is.na(BESTLAT) | is.na(BESTLON)])
      cat(na_num, '\n')
    }
    
    schools <- schools[NCESSCH=='90483001849', BESTLAT:=41.543699]
    schools <- schools[NCESSCH=='90483001849', BESTLON:=-73.047156]
    na_num <- nrow(schools[is.na(BESTLAT) | is.na(BESTLON)])
    cat(na_num, '\n')
    schools <- schools[!(is.na(BESTLAT) | is.na(BESTLON))]
    
    ## Assign LEAID.1989 to post2014 data
    dt_lea89 <- unique(schools[!is.na(LEAID.1989)][,.(NCESSCH, LEAID.1989.Join = LEAID.1989)])
    setkey(dt_lea89, NCESSCH)
    setkey(schools, NCESSCH)
    schools <- dt_lea89[schools]
    schools <- schools[is.na(LEAID.1989), LEAID.1989:=LEAID.1989.Join]
    
    ## For 'new' schools post 2014, intersect to LEAID.1989
    schools_missing <- schools[is.na(LEAID.1989), .SD[1], by=NCESSCH][, .(NCESSCH, BESTLAT, BESTLON)]
    shps <- get_district_maps()
    shps_89 <- shps$`1989`  
    shps_89 <- shps_89$map
    coords <- cbind(Longitude = as.numeric(as.character(schools_missing$BESTLON)), 
                    Latitude = as.numeric(as.character(schools_missing$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_missing, proj4string = sp::CRS(proj_env))
    tmp <- sp::over(schools_pts, shps_89)
    tmp <- as.data.table(tmp)
    schools_missing <- data.table(NCESSCH =schools_missing$NCESSCH, LEAID.1989.JOIN=tmp$GEOID)
    setkey(schools_missing, NCESSCH)
    setkey(schools, NCESSCH)
    schools <- schools_missing[schools]
    schools <- schools[is.na(LEAID.1989), LEAID.1989:=LEAID.1989.JOIN]
    cat(nrow(schools[is.na(LEAID.1989)]), '\n')
    
    # For those still unmatched, assign to nearest LEAID.1989
    schools_missing <- schools[is.na(LEAID.1989), .SD[1], by=NCESSCH][, .(NCESSCH, BESTLAT, BESTLON)]
    coords <- cbind(Longitude = as.numeric(as.character(schools_missing$BESTLON)), 
                    Latitude = as.numeric(as.character(schools_missing$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_missing, proj4string = sp::CRS(proj_env))
    utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    crs <- CRS(sprintf(utmStr, 32))
    shps_89UTM <- spTransform(shps_89, crs)
    pts_UTM <- spTransform(schools_pts, crs)
    for(i_school in 1:nrow(schools_missing)){
      tmp_LEADID.1989 <- as.character(shps_89UTM[which.min(rgeos::gDistance(pts_UTM[i_school,], shps_89UTM, byid=TRUE)),]@data$shp_LEA)
      schools <- schools[NCESSCH==pts_UTM[i_school,]$NCESSCH, LEAID.1989 := tmp_LEADID.1989]
    }
    schools_missing <- schools[is.na(LEAID.1989), .SD[1], by=NCESSCH][, .(NCESSCH, BESTLAT, BESTLON)]
    cat('Number missing LEAD.1989:', nrow(schools_missing), '\n')
    
    # Update the lifted/year lifted restrictions for post 2014
    dt_restrictions <- schools[order(NCESSCH, -YEAR)][!is.na(LIFTED) & LIFTED != 'NONE', .SD[1], by=LEAID.1989, .SDcols=c('YEAR.LIFTED', 'LIFTED')]
    setnames(dt_restrictions, c('YEAR.LIFTED', 'LIFTED'), c('YEAR.LIFTED.JOIN', 'LIFTED.JOIN'))
    setkey(dt_restrictions, LEAID.1989)
    setkey(schools, LEAID.1989)
    schools <- dt_restrictions[schools]
    schools <- schools[is.na(YEAR.LIFTED) & !is.na(YEAR.LIFTED.JOIN), YEAR.LIFTED:=YEAR.LIFTED.JOIN]
    schools <- schools[is.na(LIFTED) & !is.na(LIFTED.JOIN), LIFTED:=LIFTED.JOIN]
    
    # Drop columns with 'join'
    cols_join <- names(schools)[stringr::str_detect(names(schools), '(?i)join')]
    cols_impute <- names(schools)[stringr::str_detect(names(schools), '((?i)((white|black)\\_))|(lag|lead)')]
    cols_drop <- c(cols_join, cols_impute)
    cols <- names(schools)
    cols_keep <- cols[!(cols %in% cols_drop)]
    schools <- schools[, cols_keep, with=FALSE]
    # Consolidate and georeference
    schools_unique <- unique(schools[, .(LEAID, NCESSCH, YEAR, WHITE, BLACK, BESTLON, BESTLAT)])
    coords <- cbind(Longitude = as.numeric(as.character(schools_unique$BESTLON)), 
                    Latitude = as.numeric(as.character(schools_unique$BESTLAT)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_unique, proj4string = sp::CRS(proj_env))
    #rgdal::writeOGR(schools_pts, '~/Downloads/', 'tmp_schools_pts', driver='ESRI Shapefile')
    #schools_pts@data 
    l_schools <- list(dt=schools, pts=schools_pts)
    saveRDS(l_schools, file = schools_location)
  } else {
    l_schools <- readRDS(schools_location)
  }
  return(l_schools)
}
