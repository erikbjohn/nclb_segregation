get_schools_post2014 <- function(){
  schools_post2014_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/schools_post2014.rds'
  
  ### NEED 2015-2018 HERE:https://nces.ed.gov/ccd/pubschuniv.asp
  
  if(!file.exists(schools_post2014_location)){
    # Schools locations
    proj_env <- get_proj_env()
    schools_old <- readRDS('~/Dropbox/pkg.data/nclb_segregation/Clean/ejData_never_delete_me.rds')
    schools_old <- schools_old[, NCESSCH:=as.character(NCESSCH)]
    
    #l_schools_new <- fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/')
    #2014-2015
    s_1415_mem <- fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/ccd_sch_052_1415_w_0216161a.txt')  # Membership
    s_1415_mem <- s_1415_mem[, NCESSCH:=as.character(NCESSCH)]
    setkey(s_1415_mem, NCESSCH)
    s_1415_geo <- as.data.table(readxl::read_xlsx('~/Dropbox/pkg.data/nclb_segregation/SchoolData/EDGE_GEOIDS_201415_PUBLIC_SCHOOL_xlsx.xlsx'))
    s_1415_geo <- s_1415_geo[, NCESSCH:=as.character(as.numeric(NCESSCH))]
    setkey(s_1415_geo, NCESSCH)
    s_1415_dir <- fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/ccd_sch_029_1415_w_0216601a.txt')
    s_1415_dir <- s_1415_dir[,NCESSCH:=as.character(NCESSCH)]
    setkey(s_1415_dir, NCESSCH)
    s_1415_char <- fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/ccd_sch_059_1516_w_2a_011717.csv')
    s_1415_char <- s_1415_char[, NCESSCH:=as.character(NCESSCH)]
    setkey(s_1415_char, NCESSCH)
    s_1415_staff <- fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/ccd_sch_059_1415_w_0216161a.txt')
    s_1415_staff <- s_1415_staff[, NCESSCH:=as.character(NCESSCH)]
    setkey(s_1415_staff, NCESSCH)
    
    s_1415 <- s_1415_geo[s_1415_mem]
    s_1415 <- s_1415_dir[s_1415]
    s_1415 <- s_1415_char[s_1415]
    s_1415 <- s_1415_staff[s_1415]
    
    s_1415 <- s_1415[, .(LEAID, NCESSCH, 
                         BESTLAT = LATCODE, BESTLON = LONGCODE,
                         FTE, GSHI, GSLO, PHONE, SCHNO=SCHID,
                         TYPE = SCH_TYPE, STATUS = SY_STATUS,
                         ASIAN = AS, BLACK=BL, HISP = HI,
                         G01, G02, G03, G04, G05, G06,
                         G07, G08, G09, G10, G11, G12, KG,
                         LEANM = LEA_NAME, MEMBER, PK, 
                         SCHNAM = SCH_NAME, SEASCH = ST_SCHID,
                         STID = ST_LEAID, WHITE = WH, YEAR = 2015)]
    
    # 2015-2016 data
    s_1516_files  <- list.files('~/Dropbox/pkg.data/nclb_segregation/SchoolData/s_1516',recursive = TRUE, full.names = TRUE)
    s_1516_csvs <- s_1516_files[stringr::str_detect(s_1516_files, '\\.csv$')]
    l_s_1516 <- lapply(s_1516_csvs, fread)
    lapply(l_s_1516, function(x) setkey(x, NCESSCH))
    dt <- l_s_1516[[1]]
    for(i_dt in 1:(length(l_s_1516)-1)){
      dt_ind <- i_dt+1
      dt_new <- l_s_1516[[dt_ind]]
      dt <- dt_new[dt]
    }
    s_1516 <- dt
    rm(dt)
    s_1516 <- s_1516[, NCESSCH:=as.character(NCESSCH)]
    setkey(s_1516, NCESSCH)
    s_1516_geo <- as.data.table(readxl::read_xlsx('~/Dropbox/pkg.data/nclb_segregation/SchoolData/s_1516/EDGE_GEOCODE_PUBLICSCH_1516/EDGE_GEOCODE_PUBLICSCH_1516.xlsx'))
    s_1516_geo <- s_1516_geo[, NCESSCH:=as.character(as.numeric(NCESSCH))]
    setkey(s_1516_geo, NCESSCH)
    s_1516 <- s_1516_geo[s_1516]
    
    s_1516 <- s_1516[, .(LEAID, #
                         NCESSCH, #
                         BESTLAT = LAT1516, #
                         BESTLON = LON1516, #
                         LEAID.1989 = NA,
                         TOTETH = TOTAL, #
                         FTE, #
                         GSHI,  #
                         GSLO, # 
                         PHONE, # 
                         SCHNO = SCHID, # 
                         TYPE = SCH_TYPE, # 
                         STATUS = SY_STATUS, # 
                         ASIAN = AS, # 
                         BLACK  = BL, # 
                         HISP = HI, # 
                         G01, G02, G03, G04, G05, G06, #
                         G07, G08, G09, G10, G11, G12,  #
                         KG, #
                         LEANM = LEA_NAME, #
                         MEMBER,  #
                         PK,  # 
                         SCHNAM = SCH_NAME, #
                         SEASCH = ST_SCHID, #
                         STID = ST_LEAID, #
                         WHITE = WH, #
                         UG, #
                         YEAR.LIFTED = NA,
                         LIFTED = NA,
                         YEAR = 2016)]
    
    
    s_1617_files  <- list.files('~/Dropbox/pkg.data/nclb_segregation/SchoolData/s_1617',recursive = TRUE, full.names = TRUE)
    s_1617_csvs <- s_1617_files[stringr::str_detect(s_1617_files, '\\.csv$')]
    l_s_1617 <- lapply(s_1617_csvs, fread)
    
    dt_lunch_work <- l_s_1617[[2]]
    dt_lunch_work <- unique(dt_lunch_work[DATA_GROUP %in% 'Direct Certification', .(NCESSCH, LUNCH=STUDENT_COUNT)])
    setkey(dt_lunch_work, NCESSCH)
    l_s_1617[[2]] <- dt_lunch_work
    
    dt_mem_work <- l_s_1617[[3]]
    dt_grades <- dt_mem_work[RACE_ETHNICITY %in% 'No Category Codes' &
                                    SEX %in% 'No Category Codes' &
                                    grepl('Grade|Kinderga|Ungraded', GRADE), .(NCESSCH, GRADE, STUDENT_COUNT)]
    dt_grades <- dt_grades[, GRADE:=stringr::str_replace(GRADE, 'Grade ', 'G')]
    dt_grades <- dt_grades[GRADE=='Kindergarten', GRADE:='KG']
    dt_grades <- dt_grades[GRADE=='Pre-Kindergarten', GRADE:='PK']
    dt_grades <- dt_grades[GRADE=='Ungraded', GRADE:='UG']
    dt_grades <- tidyr::spread(dt_grades, GRADE, STUDENT_COUNT)
    dt_grades <- as.data.table(dt_grades)
    
    dt_member <- dt_mem_work[RACE_ETHNICITY %in% 'No Category Codes' &
                               SEX %in% 'No Category Codes' &
                               GRADE %in% 'No Category Codes' &
                             TOTAL_INDICATOR %in% 'Derived - Education Unit Total minus Adult Education Count',
                             .(NCESSCH, MEMBER = STUDENT_COUNT)]
    setkey(dt_member, NCESSCH)
    
    dt_race <- dt_mem_work[GRADE %in% 'No Category Codes' &
                             grepl('Black|Hispanic|White|Asian|Pacific|Indian|Two or more', RACE_ETHNICITY),
                           .(STUDENT_COUNT = sum(STUDENT_COUNT)),
                           by=.(NCESSCH, RACE_ETHNICITY)]
    dt_race <- tidyr::spread(dt_race, RACE_ETHNICITY, STUDENT_COUNT)
    dt_race <- as.data.table(dt_race)
    setnames(dt_race, names(dt_race), c('NCESSCH', 'AM', 'ASIAN', 'BLACK', 'HISP', 'HP', 'TR', 'WHITE'))
    dt_race <- dt_race[, TOTETH := rowSums(.SD, na.rm=TRUE), .SDcols= c('AM', 'ASIAN', 'BLACK', 'HISP', 'WHITE', 'HP', 'TR', 'WHITE')]
    dt_race <- dt_race[, .(NCESSCH, ASIAN, BLACK, HISP, WHITE, TOTETH)]
    
    setkey(dt_grades, NCESSCH)
    setkey(dt_race, NCESSCH)
    setkey(dt_member, NCESSCH)
    dt_grades_race <- dt_grades[dt_race]
    l_s_1617[[3]] <- dt_member[dt_grades_race]
    
    lapply(l_s_1617, function(x) setkey(x, NCESSCH))
    dt <- l_s_1617[[1]] # Largest file
    for(i_dt in 1:(length(l_s_1617)-1)){
      dt_ind <- i_dt+1
      dt_new <- l_s_1617[[dt_ind]]
      dt <- dt_new[dt]
    }
    s_1617 <- dt
    rm(dt)
    s_1617 <- s_1617[, NCESSCH:=as.character(NCESSCH)]
    setkey(s_1617, NCESSCH)
  s_1617_geo <- as.data.table(readxl::read_xlsx('~/Dropbox/pkg.data/nclb_segregation/SchoolData/s_1617/EDGE_GEOCODE_PUBLICSCH_1617/EDGE_GEOCODE_PUBLICSCH_1617.xlsx'))
    s_1617_geo <- s_1617_geo[, NCESSCH:=as.character(as.numeric(NCESSCH))]
    setkey(s_1617_geo, NCESSCH)
    s_1617 <- s_1617_geo[s_1617]
    
    s_1617 <- s_1617[, .(LEAID, #
                         NCESSCH, #
                         BESTLAT = LAT, #
                         BESTLON = LON, #
                         LEAID.1989 = NA,
                         TOTETH, #
                         FTE = TEACHERS,#
                         GSHI,  #
                         GSLO, # 
                         PHONE, # 
                         SCHNO = SCHID, # 
                         TYPE = SCH_TYPE, # 
                         STATUS = SY_STATUS, # 
                         ASIAN, # 
                         BLACK, # 
                         HISP, # 
                         G01 = G1,
                         G02 = G2,
                         G03 = G3,
                         G04 = G4,
                         G05 = G5,
                         G06 = G6, #
                         G07 = G7,
                         G08 = G8,
                         G09 = G9,
                         G10,
                         G11,
                         G12,  #
                         KG, #
                         LEANM = LEA_NAME, #
                         MEMBER,  #
                         PK,  # 
                         SCHNAM = SCH_NAME, #
                         SEASCH = ST_SCHID, #
                         STID = ST_LEAID, #
                         WHITE, #
                         UG = NA, #
                         YEAR.LIFTED = NA,
                         LIFTED = NA,
                         YEAR = 2017)]
    
    
    
    #----------------#
    
    
    
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
