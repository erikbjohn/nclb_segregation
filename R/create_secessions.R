create_secessions <- function(){
  create_secessions_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/create_secessions.rds'
 
  library(sp)
  if(!file.exists(get_secessions_location)){
    
    l_district_maps <- get_district_maps()
    l_schools <- get_schools()
   
    
       
    # All shapes imports
    # shp_full <- readRDS('~/Dropbox/pkg.data/nclb_segregation/SchoolData/district_shp_files/shp_districts_full.rds')
    
    # 1989/90 map is weird because school district ids (LEAID) change
    
    ii
    l_pts <- list()
    # Overlay schools on 1989/90 map (NEED TO UPDATE FOR district_maps list instead.)
    dt_89_90 <- sp::over(schools_pts, shp_89_90)
    head(dt_89_90)
    head(schools_pts)
    dt_89_90 <- cbind(dt_89_90, schools_pts@data, year='8990')
    l_pts$dt_89_90 <- dt_89_90[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 1995/96 map
    dt_95_96 <- sp::over(schools_pts, shp_95_96)
    head(dt_95_96)
    head(schools_pts)
    dt_95_96 <- cbind(dt_95_96, schools_pts@data, year='9596')
    l_pts$dt_95_96 <- dt_95_96[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 1997/98 map
    dt_97_98 <- sp::over(schools_pts, shp_97_98)
    dt_97_98 <- cbind(dt_97_98, schools_pts@data, year='9798')
    l_pts$dt_97_98 <- dt_97_98[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 1999/00 map
    dt_99_00 <- sp::over(schools_pts, shp_99_00)
    dt_99_00 <- cbind(dt_99_00, schools_pts@data, year='9900')
    l_pts$dt_99_00 <- dt_99_00[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2001/02 map
    dt_01_02 <- sp::over(schools_pts, shp_01_02)
    dt_01_02 <- cbind(dt_01_02, schools_pts@data, year='0102')
    l_pts$dt_01_02 <- dt_01_02[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2003/04 map
    dt_03_04 <- sp::over(schools_pts, shp_03_04)
    dt_03_04 <- cbind(dt_03_04, schools_pts@data, year='0304')
    l_pts$dt_03_04 <- dt_03_04[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2005/06 map
    dt_05_06 <- sp::over(schools_pts, shp_05_06)
    dt_05_06 <- cbind(dt_05_06, schools_pts@data, year='0506')
    l_pts$dt_05_06 <- dt_05_06[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2007/08 map
    dt_07_08 <- sp::over(schools_pts, shp_07_08)
    dt_07_08 <- cbind(dt_07_08, schools_pts@data, year='0708')
    l_pts$dt_07_08 <- dt_07_08[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2009/10 map
    dt_09_10 <- sp::over(schools_pts, shp_09_10)
    dt_09_10 <- cbind(dt_09_10, schools_pts@data, year='0910')
    l_pts$dt_09_10 <- dt_09_10[, .(GEOID = GEOID10, NCESSCH, year)]
    
    # Overlay schools on 2011/12 map
    dt_11_12 <- sp::over(schools_pts, shp_11_12)
    dt_11_12 <- cbind(dt_11_12, schools_pts@data, year='1112')
    l_pts$dt_11_12 <- dt_11_12[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2013/2014  map
    dt_13_14 <- sp::over(schools_pts, shp_13_14)
    dt_13_14 <- cbind(dt_13_14, schools_pts@data, year='1314')
    l_pts$dt_13_14 <- dt_13_14[, .(GEOID, NCESSCH, year)]
    
    # Overlay schools on 2013/2014  map
    dt_15_16 <- sp::over(schools_pts, shp_15_16)
    dt_15_16 <- cbind(dt_15_16, schools_pts@data, year='1516')
    l_pts$dt_15_16 <- dt_15_16[, .(GEOID, NCESSCH, year)]
    
    dt_pts_dists <- rbindlist(l_pts, use.names = TRUE, fill = TRUE)
  
    # Start checking to identify which schools are in one school district for the whole time period
    l_status <- list()
    n_years <- length(unique(dt_pts_dists$year))
    dt_pts_dists <- dt_pts_dists[, n_location_years:=.N, by=.(NCESSCH, GEOID)]
    saveRDS(dt_pts_dists, file='~/Dropbox/pkg.data/nclb_segregation/schools_and_districts_years.rds')
    dt_analyze <- copy(dt_pts_dists)
    table(dt_pts_dists$n_location_years)
    # No change
    l_status$no_change <- unique(dt_pts_dists[n_location_years==n_years]$NCESSCH)
    dt_analyze <- copy(dt_analyze[!(NCESSCH %in% l_status$no_change)])
    
    # For each school that has changed school district, find out year(s) and reasons
    school <- dt_analyze[NCESSCH=='10000600123']
    plot(shp_03_04[shp_03_04@data$GEOID=='0100006',], col=rgb(red=1, green=0, blue=0, alpha=0.3))
    plot(shp_15_16[shp_15_16@data$GEOID=='0100012',], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), add=TRUE)
    plot(shp_05_06[shp_05_06@data$GEOID=='0199016',], col = rgb(red = 0, green = 1, blue = 0, alpha = 0.1), add=TRUE)
    plot(schools_pts[schools_pts@data$NCESSCH=='10000600123',], col = rgb(red = 0, green = 1, blue = 1, alpha = 1))
    
  }
    

    
}
