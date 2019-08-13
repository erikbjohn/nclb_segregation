get_secessions <- function(){
  get_secessions_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/secessions.rds'
  if(!file.exists(get_secessions_location)){
    schools <- create_segregation_indices()
    raw <- fread('~/Dropbox/pkg.data/nclb_segregation/raw/secession/school_district_secessions_since2000_2019.csv')
    setnames(raw, names(raw), c('state_name', 'secession_status_full', 'seceding_district_ncesid', 
                                'seceding_district', 'seceded_from_district_ncesid', 'sceded_from_behind_district'))
    raw$secession_year <- as.numeric(stringr::str_extract(raw$secession_status_full, stringr::regex('(?<=\\().+(?=\\))')))
    raw$secession_status <- sapply(stringr::str_split(raw$secession_status_full, ' \\('), '[[', 1)
    raw$secession_status_full <- NULL
    raw <- raw[, seceding_district_ncesid := stringr::str_pad(string = seceding_district_ncesid, side = 'l', width = 7, pad='0')]
    raw <- raw[, seceded_from_district_ncesid := stringr::str_pad(string = seceded_from_district_ncesid, side = 'l', width = 7, pad='0')]
    
    saveRDS(raw, get_secessions_location)
  } else{
    raw <- readRDS(get_secessions_location)
  }
  return(raw)
}
#   # Pre 2000 data
#   schools <- create_segregation_indices()
#   state_fips <- get_state_fips()
#   l_districts <- get_district_maps()
#   
#   # New spatially defined school districts (no charters)
#   sd_shapes <- unique(schools[index_geo_year=='contemp'][, .(LEAID=index_geo_id, YEAR, EXISTS=1)])
#   sd_shapes <- sd_shapes[, STFIPS:=stringr::str_sub(LEAID, 1, 2)]
#   sd_shapes <- sd_shapes[STFIPS %in% state_fips$STFIPS]
#   sd_shapes$STFIPS <- NULL
#   sd_shapes <- tidyr::spread(sd_shapes, YEAR, EXISTS, fill=0)
#   sd_shapes <- as.data.table(tidyr::gather(sd_shapes, YEAR, EXISTS, -LEAID))
#   sd_shapes <- sd_shapes[, YEAR:=as.numeric(YEAR)]
#   sd_shapes <- sd_shapes[, STFIPS:=stringr::str_sub(LEAID, 1, 2)]
#   setkey(sd_shapes, LEAID, YEAR)
#   
#   # Track new districts
#   sd_shapes <- sd_shapes[, EXISTS_lag:=shift(EXISTS, 1, type='lag'), by=LEAID]
#   sd_shapes$new_district <- 0
#   sd_shapes <- sd_shapes[EXISTS == 1 & EXISTS_lag == 0, new_district:=1]
#   
#   # Disappearing districts
#   sd_shapes <- sd_shapes[, EXISTS_lead:=shift(EXISTS, 1, type='lead'), by=LEAID]
#   sd_shapes$gone_district <- 0
#   sd_shapes <- sd_shapes[EXISTS == 1 & EXISTS_lead == 0, gone_district:=1]
#   
#   # Drop lead and lags
#   sd_shapes <- sd_shapes[!is.na(EXISTS_lag) & !is.na(EXISTS_lead)]
#   
#   # Use only districts that came into the database and never left
#   sd_shapes <- sd_shapes[, new_count:=sum(new_district), by=LEAID]
#   sd_shapes <- sd_shapes[, gone_count:=sum(gone_district), by=LEAID]
# 
#   # Focus on those years that exist
#   sd_shapes <- sd_shapes[EXISTS==1 & new_district==1]
#   sd_shapes <- sd_shapes[, .(STFIPS, LEAID, YEAR)]
#   setkey(sd_shapes, STFIPS)
#   setkey(state_fips, STFIPS)
#   sd_shapes <- state_fips[sd_shapes]
#   
#   # By year by state
#   sd_year_shapes <- unique(sd_shapes[,.(STATE_NAME, new=.N), by=.(STFIPS, YEAR)][order(YEAR, -new)][YEAR>1990])
#   View(sd_year_shapes)
#   
#   # Now, need to actually see if just simple renaming (Coffee Creek Elementary 0609240 -> Trinity Alps Unified School District (0606007))
#   # Or if actually new district (nebraska case)
#   
#   n_sds <- nrow(sd_shapes)
#   # Not all l_shps used in maps
#   dt <- data.table(sYear = sapply(l_districts, '[[', 1),
#              Year = sapply(l_districts, '[[', 2))
#               
#   
#   match_descrs <- data.table()
#   for(iShp in 1:n_sds){
#     sd_shape <- sd_shapes[iShp]
#     district_leaid <- sd_shape$LEAID
#     district_year <- sd_shape$YEAR
#     district_maps <- l_districts[which(names(l_districts) == district_year)][[1]]$map
#     district_imap <- l_districts[which(names(l_districts) == district_year)][[1]]$iMap
#     district_map <- district_maps[district_maps@data$GEOID==district_leaid,]
#     data.table(sapply(l_districts, '[[', 1))
#     if(district_year < 1996){
#       # No previous maps
#       match_descr <- data.table(LEADID=district_leaid, YEAR=district_year, match='No Map')
#     } else {
#       lag_map_indices <- sapply(l_districts, '[[', 4)
#       lag_map_index <- which.max(lag_map_indices == (district_imap-1))[1]
#       lag_map <- l_districts[lag_map_index][[1]]$map
#       
#     }
#    
#     
#     shp_current <- 
#   }
#   
#   
#   #### FOR ACTUAL 
#   
#   dt <- schools$dt
#   
#   # Create district ids by year
#   sdists <- unique(dt[,.(LEAID, YEAR, EXISTS=1)])
#   sdists <- tidyr::spread(sdists, YEAR, EXISTS, fill=0)
#   sdists <- as.data.table(tidyr::gather(sdists, YEAR, EXISTS, -LEAID))
#   sdists <- sdists[, YEAR:=as.numeric(YEAR)]
#   sdists <- sdists[, STFIPS:=stringr::str_sub(LEAID, 1, 2)]
#   sdists <- sdists[STFIPS %in% state_fips$STFIPS]
#   setkey(sdists, LEAID, YEAR)
#   
#   # Track new districts
#   sdists <- sdists[, EXISTS_lag:=shift(EXISTS, 1, type='lag'), by=LEAID]
#   sdists$new_district <- 0
#   sdists <- sdists[EXISTS == 1 & EXISTS_lag == 0, new_district:=1]
#   
#   # Disappearing districts
#   sdists <- sdists[, EXISTS_lead:=shift(EXISTS, 1, type='lead'), by=LEAID]
#   sdists$gone_district <- 0
#   sdists <- sdists[EXISTS == 1 & EXISTS_lead == 0, gone_district:=1]
#   
#   # Drop lead and lags
#   sdists <- sdists[!is.na(EXISTS_lag) & !is.na(EXISTS_lead)]
#   
#   # Use only districts that came into the database and never left
#   sdists <- sdists[, new_count:=sum(new_district), by=LEAID]
#   sdists <- sdists[, gone_count:=sum(gone_district), by=LEAID]
#   gone_districts <- unique(sdists[gone_count>0]$LEAID)
#   sdists <- sdists[!(LEAID %in% gone_districts)]
# 
#   # districts new in their year of entry
#   dists_new <- sdists[new_count==1 & new_district==1& YEAR > 1990]
#   
#   # Summarize new district counts by state by year
#   setkey(dists_new, STFIPS)
#   setkey(state_fips, STFIPS)
#   dists_new <- state_fips[dists_new]
#   
#   years_state_new <- data.table(table(unique(dists_new[,.(LEAID, STFIPS, YEAR)])[,.(STFIPS, YEAR)])))
#   years_state_new <- state_fips[years_state_new]
#   View(years_state_new[order(YEAR, -N)])
#   state_new <- years_state_new[, .(n_state_new = sum(N)), by=.(STFIPS, STATE_NAME)]
#   View(state_new[order(-n_state_new)])
#   
#   years_new <- dists_new[, .(count=.N), by=YEAR][order(YEAR)]
#   plot(years_new)
#   
#   # Explore new districts
# 
#   table(dists_new[, .(STFIPS, YEAR)])
#   
#   
#   dt_new <- data.table(new_dists=dt_new)
#   plot(dt_new)
#   
#   
#   # l_district_maps <- get_district_maps()
#   # l_schools <- get_schools()
#   # 
#   # 
#   #    
#   # # All shapes imports
#   # # shp_full <- readRDS('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/shp_districts_full.rds')
#   # 
#   # # 1989/90 map is weird because school district ids (LEAID) change
#   # 
#   # ii
#   # l_pts <- list()
#   # # Overlay schools on 1989/90 map (NEED TO UPDATE FOR district_maps list instead.)
#   # dt_89_90 <- sp::over(schools_pts, shp_89_90)
#   # head(dt_89_90)
#   # head(schools_pts)
#   # dt_89_90 <- cbind(dt_89_90, schools_pts@data, year='8990')
#   # l_pts$dt_89_90 <- dt_89_90[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 1995/96 map
#   # dt_95_96 <- sp::over(schools_pts, shp_95_96)
#   # head(dt_95_96)
#   # head(schools_pts)
#   # dt_95_96 <- cbind(dt_95_96, schools_pts@data, year='9596')
#   # l_pts$dt_95_96 <- dt_95_96[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 1997/98 map
#   # dt_97_98 <- sp::over(schools_pts, shp_97_98)
#   # dt_97_98 <- cbind(dt_97_98, schools_pts@data, year='9798')
#   # l_pts$dt_97_98 <- dt_97_98[, .(GEOID, NCESSCH, year)]sdists[new_district==1, .(YEAR)
#   
#   dt_new <- table(sdists[new_district==1, .(YEAR)])
#   dt_new <- data.table(new_dists=dt_new)
#   plot(dt_new)
#   # l_district_maps <- get_district_maps()
#   # l_schools <- get_schools()
#   # 
#   # 
#   #    
#   # # All shapes imports
#   # # shp_full <- readRDS('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/shp_districts_full.rds')
#   # 
#   # # 1989/90 map is weird because school district ids (LEAID) change
#   # 
#   # ii
#   # l_pts <- list()
#   # # Overlay schools on 1989/90 map (NEED TO UPDATE FOR district_maps list instead.)
#   # dt_89_90 <- sp::over(schools_pts, shp_89_90)
#   # head(dt_89_90)
#   # head(schools_pts)
#   # dt_89_90 <- cbind(dt_89_90, schools_pts@data, year='8990')
#   # l_pts$dt_89_90 <- dt_89_90[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 1995/96 map
#   # dt_95_96 <- sp::over(schools_pts, shp_95_96)
#   # head(dt_95_96)
#   # head(schools_pts)
#   # dt_95_96 <- cbind(dt_95_96, schools_pts@data, year='9596')
#   # l_pts$dt_95_96 <- dt_95_96[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 1997/98 map
#   # dt_97_98 <- sp::over(schools_pts, shp_97_98)
#   # dt_97_98 <- cbind(dt_97_98, schools_pts@data, year='9798')
#   # l_pts$dt_97_98 <- dt_97_98[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 1999/00 map
#   # dt_99_00 <- sp::over(schools_pts, shp_99_00)
#   # dt_99_00 <- cbind(dt_99_00, schools_pts@data, year='9900')
#   # l_pts$dt_99_00 <- dt_99_00[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2001/02 map
#   # dt_01_02 <- sp::over(schools_pts, shp_01_02)
#   # dt_01_02 <- cbind(dt_01_02, schools_pts@data, year='0102')
#   # l_pts$dt_01_02 <- dt_01_02[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2003/04 map
#   # dt_03_04 <- sp::over(schools_pts, shp_03_04)
#   # dt_03_04 <- cbind(dt_03_04, schools_pts@data, year='0304')
#   # l_pts$dt_03_04 <- dt_03_04[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2005/06 map
#   # dt_05_06 <- sp::over(schools_pts, shp_05_06)
#   # dt_05_06 <- cbind(dt_05_06, schools_pts@data, year='0506')
#   # l_pts$dt_05_06 <- dt_05_06[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2007/08 map
#   # dt_07_08 <- sp::over(schools_pts, shp_07_08)
#   # dt_07_08 <- cbind(dt_07_08, schools_pts@data, year='0708')
#   # l_pts$dt_07_08 <- dt_07_08[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2009/10 map
#   # dt_09_10 <- sp::over(schools_pts, shp_09_10)
#   # dt_09_10 <- cbind(dt_09_10, schools_pts@data, year='0910')
#   # l_pts$dt_09_10 <- dt_09_10[, .(GEOID = GEOID10, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2011/12 map
#   # dt_11_12 <- sp::over(schools_pts, shp_11_12)
#   # dt_11_12 <- cbind(dt_11_12, schools_pts@data, year='1112')
#   # l_pts$dt_11_12 <- dt_11_12[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2013/2014  map
#   # dt_13_14 <- sp::over(schools_pts, shp_13_14)
#   # dt_13_14 <- cbind(dt_13_14, schools_pts@data, year='1314')
#   # l_pts$dt_13_14 <- dt_13_14[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2013/2014  map
#   # dt_15_16 <- sp::over(schools_pts, shp_15_16)
#   # dt_15_16 <- cbind(dt_15_16, schools_pts@data, year='1516')
#   # l_pts$dt_15_16 <- dt_15_16[, .(GEOID, NCESSCH, year)]
#   # 
#   # dt_pts_dists <- rbindlist(l_pts, use.names = TRUE, fill = TRUE)
#   # 
#   # # Start checking to identify which schools are in one school district for the whole time period
#   # l_status <- list()
#   # n_years <- length(unique(dt_pts_dists$year))
#   # dt_pts_dists <- dt_pts_dists[, n_location_years:=.N, by=.(NCESSCH, GEOID)]
#   # saveRDS(dt_pts_dists, file='~/Dropbox/pkg.data/nclb_segregation/schools_and_districts_years.rds')
#   # dt_analyze <- copy(dt_pts_dists)
#   # table(dt_pts_dists$n_location_years)
#   # # No change
#   # l_status$no_change <- unique(dt_pts_dists[n_location_years==n_years]$NCESSCH)
#   # dt_analyze <- copy(dt_analyze[!(NCESSCH %in% l_status$no_change)])
#   # 
#   # # For each school that has changed school district, find out year(s) and reasons
#   # school <- dt_analyze[NCESSCH=='10000600123']
#   # plot(shp_03_04[shp_03_04@data$GEOID=='0100006',], col=rgb(red=1, green=0, blue=0, alpha=0.3))
#   # plot(shp_15_16[shp_15_16@data$GEOID=='0100012',], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), add=TRUE)
#   # plot(shp_05_06[shp_05_06@data$GEOID=='0199016',], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.1), add=TRUE)
#   # plot(schools_pts[schools_pts@data$NCESSCH=='10000600123',], col = rgb(red = 0, green = 1, blue = 1, alpha = 1))
#   
# }
# 
# 
# 
# 
#   # 
#   # # Overlay schools on 1999/00 map
#   # dt_99_00 <- sp::over(schools_pts, shp_99_00)
#   # dt_99_00 <- cbind(dt_99_00, schools_pts@data, year='9900')
#   # l_pts$dt_99_00 <- dt_99_00[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2001/02 map
#   # dt_01_02 <- sp::over(schools_pts, shp_01_02)
#   # dt_01_02 <- cbind(dt_01_02, schools_pts@data, year='0102')
#   # l_pts$dt_01_02 <- dt_01_02[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2003/04 map
#   # dt_03_04 <- sp::over(schools_pts, shp_03_04)
#   # dt_03_04 <- cbind(dt_03_04, schools_pts@data, year='0304')
#   # l_pts$dt_03_04 <- dt_03_04[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2005/06 map
#   # dt_05_06 <- sp::over(schools_pts, shp_05_06)
#   # dt_05_06 <- cbind(dt_05_06, schools_pts@data, year='0506')
#   # l_pts$dt_05_06 <- dt_05_06[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2007/08 map
#   # dt_07_08 <- sp::over(schools_pts, shp_07_08)
#   # dt_07_08 <- cbind(dt_07_08, schools_pts@data, year='0708')
#   # l_pts$dt_07_08 <- dt_07_08[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2009/10 map
#   # dt_09_10 <- sp::over(schools_pts, shp_09_10)
#   # dt_09_10 <- cbind(dt_09_10, schools_pts@data, year='0910')
#   # l_pts$dt_09_10 <- dt_09_10[, .(GEOID = GEOID10, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2011/12 map
#   # dt_11_12 <- sp::over(schools_pts, shp_11_12)
#   # dt_11_12 <- cbind(dt_11_12, schools_pts@data, year='1112')
#   # l_pts$dt_11_12 <- dt_11_12[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2013/2014  map
#   # dt_13_14 <- sp::over(schools_pts, shp_13_14)
#   # dt_13_14 <- cbind(dt_13_14, schools_pts@data, year='1314')
#   # l_pts$dt_13_14 <- dt_13_14[, .(GEOID, NCESSCH, year)]
#   # 
#   # # Overlay schools on 2013/2014  map
#   # dt_15_16 <- sp::over(schools_pts, shp_15_16)
#   # dt_15_16 <- cbind(dt_15_16, schools_pts@data, year='1516')
#   # l_pts$dt_15_16 <- dt_15_16[, .(GEOID, NCESSCH, year)]
#   # 
#   # dt_pts_dists <- rbindlist(l_pts, use.names = TRUE, fill = TRUE)
#   # 
#   # # Start checking to identify which schools are in one school district for the whole time period
#   # l_status <- list()
#   # n_years <- length(unique(dt_pts_dists$year))
#   # dt_pts_dists <- dt_pts_dists[, n_location_years:=.N, by=.(NCESSCH, GEOID)]
#   # saveRDS(dt_pts_dists, file='~/Dropbox/pkg.data/nclb_segregation/schools_and_districts_years.rds')
#   # dt_analyze <- copy(dt_pts_dists)
#   # table(dt_pts_dists$n_location_years)
#   # # No change
#   # l_status$no_change <- unique(dt_pts_dists[n_location_years==n_years]$NCESSCH)
#   # dt_analyze <- copy(dt_analyze[!(NCESSCH %in% l_status$no_change)])
#   # 
#   # # For each school that has changed school district, find out year(s) and reasons
#   # school <- dt_analyze[NCESSCH=='10000600123']
#   # plot(shp_03_04[shp_03_04@data$GEOID=='0100006',], col=rgb(red=1, green=0, blue=0, alpha=0.3))
#   # plot(shp_15_16[shp_15_16@data$GEOID=='0100012',], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), add=TRUE)
#   # plot(shp_05_06[shp_05_06@data$GEOID=='0199016',], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.1), add=TRUE)
#   # plot(schools_pts[schools_pts@data$NCESSCH=='10000600123',], col = rgb(red = 0, green = 1, blue = 1, alpha = 1))
#   

