get_private_schools <- function(){
  private_schools_location <- '~/Dropbox/pkg.data/nclb_segregation/clean/private_school.rds'
  source('R/get_proj_env.R')
  source('R/get_district_maps.R')
  
  if(!file.exists(private_schools_location)){
    proj_env <- get_proj_env()
    l_dists <- get_district_maps()
    # M. Finnegan's Cleaning code in ~/Dropbox/pkg.data/nclb_segregation/SchoolData/privates/download_clean_data.R
    dt <- readRDS('~/Dropbox/pkg.data/nclb_segregation/SchoolData/privates/privates_1993to2015.rds')
    schools_unique <- unique(dt[, .(PIN, LONGITUDE, LATITUDE)])
    schools_unique <- schools_unique[, .SD[1], by=.(PIN)]
    coords <- cbind(Longitude = as.numeric(as.character(schools_unique$LONGITUDE)), 
                    Latitude = as.numeric(as.character(schools_unique$LATITUDE)))
    schools_pts <- sp::SpatialPointsDataFrame(coords, schools_unique, proj4string = sp::CRS(proj_env))
    # Next overlay on district maps

    shp_2017 <- l_dists$`2017`$map
    schools_int <- sp::over(schools_pts, shp_2017)
    schools_int <- as.data.table(cbind(schools_pts@data, schools_int))
    schools_int <- schools_int[, .(PIN, shp_LEA_2016=shp_LEA)]
    setkey(schools_int, PIN)
    setkey(dt, PIN)
    dt <- schools_int[dt]
    
    # add in the school type variables.
    f_list <- list.files('~/Dropbox/pkg.data/nclb_segregation/raw/privates/', full.names = TRUE, pattern = 'pss')
    l_dt<- lapply(f_list, fread)
    
    # Explore weird holy spirit issue
    #hspirit_09 <- l_dt[[5]][PIN %in% '00000044']
    #hspirit_11 <- l_dt[[6]][ppin %in% '00000044']
    #l_spirit <- list(hspirit_09, hspirit_11)
    #l_spirit <- lapply(l_spirit, function(x) setnames(x, names(x), stringr::str_to_upper(names(x))))
    #dt_spirit <- rbindlist(l_spirit, use.names = TRUE, fill=TRUE)
    #View(t(dt_spirit))
    
    #caths_09 <- l_dt[[5]][orient==1]
    #caths_09$YEAR <- 2009
    #caths_11 <- l_dt[[6]][orient==1]
    #caths_11$YEAR <- 2011
    #l_caths <- list(caths_09, caths_11)
    #l_caths <- lapply(l_caths, function(x) setnames(x, names(x), stringr::str_to_upper(names(x))))
    #dt_caths <- rbindlist(l_caths, use.names = TRUE, fill=TRUE)
    #dt_caths <- dt_caths[, numYears:=.N, by=PPIN]
    #dt_caths <- dt_caths[, int_level:=as.integer(LEVEL)]
    #dt_caths_levels <- dcast(dt_caths, PPIN ~ YEAR, value.var='int_level' )
    #dt_caths_levels_table <- dt_caths_levels[, .(cathCount=.N), by=.(`2009`, `2011`)]
    #View(dt_caths_levels_table)
      
    # Extract pin, level, and p415 variable from each private school
    l_extract <- list()
    l_extract[[1]] <- l_dt[[1]][, .(PIN=PPIN, schType = P415, LEVEL=LEVEL, YEAR = 2001)]
    l_extract[[2]] <- l_dt[[2]][, .(PIN=PPIN, schType = P415, LEVEL=LEVEL, YEAR = 2003)]
    l_extract[[3]] <- l_dt[[3]][, .(PIN=ppin, schType = P415, LEVEL=level, YEAR = 2005)]
    l_extract[[4]] <- l_dt[[4]][, .(PIN=ppin, schType = P415, LEVEL=level, YEAR = 2007)]
    l_extract[[5]] <- l_dt[[5]][, .(PIN=ppin, schType = P415, LEVEL=level, YEAR = 2009)]
    l_extract[[6]] <- l_dt[[6]][, .(PIN=ppin, schType = p415, LEVEL=level, YEAR = 2011)]
    l_extract[[7]] <- l_dt[[7]][, .(PIN=PPIN, schType = P415, LEVEL=LEVEL, YEAR = 2013)]
    l_extract[[8]] <- l_dt[[8]][, .(PIN=ppin, schType = p415, LEVEL=level, YEAR = 2015)]
    l_extract[[9]] <- l_dt[[9]][, .(PIN=EPIN, schType = PSS112, LEVEL=LEVEL, YEAR = 1989)]
    l_extract[[10]] <- l_dt[[10]][, .(PIN=EPIN, schType = PSC105, LEVEL=LEVEL, YEAR = 1991)]
    l_extract[[11]] <- l_dt[[11]][, .(PIN=PPIN, schType = P415, LEVEL=LEVEL, YEAR = 1993)]
    l_extract[[12]] <- l_dt[[12]][, .(PIN=QPIN, schType = P415, LEVEL=LEVEL, YEAR = 1995)]
    l_extract[[13]] <- l_dt[[13]][, .(PIN=RPIN, schType = P415, LEVEL=LEVEL, YEAR = 1997)]
    l_extract[[14]] <- l_dt[[14]][, .(PIN=SPIN, schType = P415, LEVEL=LEVEL, YEAR = 1999)]
    
    dt_type <- rbindlist(l_extract, use.names=TRUE, fill=TRUE)
    dt_type <- dt_type[schType=='3', schType:=1]
    dt_type <- dt_type[schType==1]
    type_1_pins <- unique(dt_type$PIN)
    
    dt_level <- dt_type[, .(PIN, LEVEL, YEAR)]
    dt_level <- dt_level[YEAR > 1999]
    dt_level <- dt_level[, .(LEVEL_count=.N), by=.(PIN, LEVEL)][order(PIN)]
    dt_levels_unique <- dt_level[, .(n_LEVELS=.N), by=.(PIN)]
    dt_levels_unique <- dt_levels_unique[n_LEVELS==1]
    unique_levels_pins <- unique(dt_levels_unique$PIN)
    
    # Subset dt
    pin_subset <- data.table(PIN=c(type_1_pins, unique_levels_pins))
    pin_subset <- pin_subset[, .(pin_count=.N), by=PIN]
    pins_subset <- pin_subset[pin_count==2]$PIN
     
    dt <- dt[PIN %in% pins_subset]
    
    # Quick analysis of private school enrollments by year
    dt_year <- dt[, .(count_year=sum(NUMSTUDS_pvt)), by=.(YEAR, STATE_FIPS)]
    ggplot2::ggplot(dt_year, aes(x=YEAR, y=count_year, group=STATE_FIPS)) +
      geom_line() +
      ylim(0, max(dt_year$count_year))
    
    ggplot2::ggplot(dt_year[STATE_FIPS %in% '01'], aes(x=YEAR, y=count_year, group=1)) +
      geom_line() +
      ylim(0, max(dt_year[STATE_FIPS %in% '01']$count_year))
    
    # Examine changes from 2005-2015 in enrollment by Alabama school districts
    dt_bama <- dt[STATE_FIPS %in% '01'][, .(NUMSTUDS=sum(NUMSTUDS_pvt)), by=.(YEAR, shp_LEA_2016)]
    dt_bama <- dt_bama[is.na(NUMSTUDS), NUMSTUDS:=0]
    dt_bama <- dcast(dt_bama, shp_LEA_2016  ~ YEAR, value.var='NUMSTUDS', fill = NA)
    
    dt_bama_schools <- dt[STATE_FIPS %in% '01']
    dt_bama_schools <- dcast(dt_bama_schools, PIN ~ YEAR, value.var='NUMSTUDS_pvt')
    dt_bama_schools <- dt_bama_schools[is.na(`2015`), `2015`:=`2013`]
    
      
    # join to amanda's data
    dt_amanda_greatSchools <- fread('~/Dropbox/pkg.data/nclb_segregation/raw/privates/bama_private_amanda.csv', header = TRUE)
    dt_amanda_good_pins <- fread('~/Dropbox/pkg.data/nclb_segregation/raw/bama_private_amanda.csv', header=TRUE) 
    dt_amanda <- data.table(PIN = dt_amanda_good_pins$PIN, GreatSchools = dt_amanda_greatSchools$GreatSchools)
    setkey(dt_amanda, PIN)
    setkey(dt_bama_schools, PIN)
    dt_bama_schools <- dt_amanda[dt_bama_schools]
    dt_bama_schools <- dt_bama_schools[, .(PIN, `1993`, `1995`, `1997`, `1999`, `2001`, `2003`,
                                           `2005`, `2007`, `2009`, `2011`, `2013`, `2015`, `2019`=GreatSchools)]
    
    # Recover school names
    dt_bama_schools_names <- dt[STATE_FIPS %in% '01' & !is.na(SCHOOL_pvt)][
      order(PIN, -YEAR), .SD[1], by=.(PIN), .SDcols=c('PIN', 'SCHOOL_pvt', 'CITY')]
    setkey(dt_bama_schools_names, PIN)
    setkey(dt_bama_schools, PIN)
    dt_bama_schools <- dt_bama_schools_names[dt_bama_schools]
    
    # Recover cities
    dt_bama_cities <- dt[PIN %in% dt_bama_schools$PIN]
    dt_bama_cities$CITY <- as.character(dt_bama_cities$CITY)
    dt_bama_cities <- dt_bama_cities[CITY != '']
    dt_bama_cities <- dt_bama_cities[, .(PIN, CITY)]
    
    setkey(dt_bama_cities, PIN)
    setkey(dt_bama_schools, PIN)
    
    dt_bama_schools <- dt_bama_cities[dt_bama_schools]
    dt_bama_schools <- dt_bama_schools[,.(PIN, CITY, SCHOOL_NAME=SCHOOL_pvt, `1993`, `1995`, `1997`, `1999`, `2001`, `2003`,
                                          `2005`, `2007`, `2009`, `2011`, `2013`, `2015`, `2019`)]
    dt_bama_schools <- unique(dt_bama_schools)
    fwrite(dt_bama_schools, file='~/Dropbox/pkg.data/nclb_segregation/raw/privates/bama_private_finnegan.csv')
    
    
    
    dt_bama_schools_missing <- dt_bama_schools[is.na(`2015`)]
    
    
    dt_bama <- dt_bama[, diff_05_15:=(`2015`-`2005`)/`2005`]
    dt_bama <- dt_bama[diff_05_15>10, diff_05_15:=1]
    
    # Import Amanda's excel file that 
    
    # Covenant Christian Learning Center (Interesting: White Dropoff, Black increase)
    dt[PIN=='00002609'] 
    
    
    
    
    
    # Drop finnegan's secssion year
    dt$secession_year <- NULL
    # Next, merge to secessions
    secessions <- get_secessions()
    dt_secess <- secessions[, .(shp_LEA_2016=seceding_district_ncesid, seceding_district_ncesid, secession_status, secession_year)][!is.na(shp_LEA_2016)]
    setkey(dt_secess, shp_LEA_2016)
    setkey(dt, shp_LEA_2016)
    dt <- dt_secess[dt]

    l <- list()
    dt_secess <- dt[!is.na(seceding_district_ncesid)]
    dt_secess <- dt_secess[, years_since_secession_ej := as.integer(YEAR) - secession_year]
    
    # Balance the panel
    
    
    # Number of students in privates
    # Private school counts only for every other year
    dt_years <- sort(unique(dt_secess$years_since_secession_ej))
    dt_secess <- dt_secess[, norm_years_since_secession := 0]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(0,-1), norm_years_since_secession := 0]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-2,-3), norm_years_since_secession := -2]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-4,-5), norm_years_since_secession := -4]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-6,-7), norm_years_since_secession := -6]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-8,-9), norm_years_since_secession := -8]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-10,-11), norm_years_since_secession := -10]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-12,-13), norm_years_since_secession := -12]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-14,-15), norm_years_since_secession := -14]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-16,-17), norm_years_since_secession := -16]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-18,-19), norm_years_since_secession := -18]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-20,-21), norm_years_since_secession := -20]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-22,-23), norm_years_since_secession := -22]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(-24,-25), norm_years_since_secession := -24]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(1,2), norm_years_since_secession := 2]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(3,4), norm_years_since_secession := 4]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(5,6), norm_years_since_secession := 6]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(7,8), norm_years_since_secession := 8]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(9,10), norm_years_since_secession := 10]
    dt_secess <- dt_secess[years_since_secession_ej %in% c(11,12), norm_years_since_secession := 12]
    
  
    setkey(dt_secess, PIN, norm_years_since_secession)  
    dt_secess <- dt_secess[, lag_NUMSTUDS_pvt:=shift(NUMSTUDS_pvt, n = 1, type = 'lag')]
    dt_secess <- dt_secess[, pct_change_NUMSTUDS:=(NUMSTUDS_pvt-lag_NUMSTUDS_pvt)/lag_NUMSTUDS_pvt]
    dt_secess <- dt_secess[pct_change_NUMSTUDS < 1.0]
    hist(dt_secess$pct_change_NUMSTUDS, breaks=100)
    dist_years <- dt_year_cuts[, .(max_dist_years=max(norm_years_since_secession)), by=.(shp_LEA_2016)]
    
    low_cut <- -5
    high_cut <- 2
    dt_year_cuts <- dt_secess[norm_years_since_secession > low_cut &
                                norm_years_since_secession <= high_cut]
    #dt_year_cuts <- dt_year_cuts[norm_years_since_secession <=0, norm_years_since_secession:=0]
    
    event_base <- lm(pct_change_NUMSTUDS ~ factor(norm_years_since_secession) + RELIGIOUS_pvt + factor(shp_LEA_2016), data=dt_year_cuts)
    summary(event_base)
    
    # Make sure to balance panel by looking a disticts far enough out
    dist_years <- dt_year_cuts[, .(max_dist_years=max(norm_years_since_secession)), by=.(shp_LEA_2016)]
    
    dt_num_tots <- dt_year_cuts[, .(studs_TOT=sum(NUMSTUDS_pvt)), by=.(norm_years_since_secession)]
    
    dt_num_studs <- dt_year_cuts[, .(mean_change_num_studs=mean(pcgt_NUMSTUDS_pvt)), by=.(norm_years_since_secession)]
    
    library(car)
    scatterplot(NUMSTUDS_pvt~norm_years_since_secession|PIN, boxplots=FALSE, smooth=TRUE, reg.line=TRUE, data=dt_year_cuts)
    
    ggplot2::ggplot(dt_num_studs, aes(x=norm_years_since_secession, y=pct_change_NUMSTUDS)) + geom_point()  
    
    fixed.dum <- lm(NUMSTUDS_pvt ~norm_years_since_secession + factor(PIN), data=dt_year_cuts)
    summary(fixed.dum)
    
    random <- plm(NUMSTUDS_pvt ~norm_years_since_secession, data=dt_year_cuts, index=c('PIN'), model='random')
    summary(random)  dt <- r  dt <- r
    
    
  
    
    
    dt_spread <- dcast(dt_spread, PIN ~ schType, value.var = 'N')
    View(dt_spread)
    
    
    
    saveRDS(dt, private_schools_location)
    
    } else {
  dt <- readRDS(private_schools_location)
  }
  return(dt)
}
