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
    
    # 2016-2017
    
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
    
    
    
    # 2017-2018 (Preliminary data no race/ethnicity data for now)
    # s_1718 <- fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/ccd_sch_029_1718_w_0a_03302018_csv/ccd_sch_029_1718_w_0a_03302018.csv')
    
    # Weird column class error
    l_post14 <- list(s_1415, s_1516, s_1617)
    l_types <- list()
    years <- c('2015', '2016', '2017')
    for(iDT in 1:length(l_post14)){
      dt_type <- data.table(colClass = sapply(l_post14[[iDT]], class))
      dt_type$colName <- names(l_post14[[iDT]])
      setnames(dt_type, 'colClass', paste0('colClass', years[iDT]))  
      setkey(dt_type, colName)
      l_types[[iDT]] <- dt_type
      dt_type <- NULL
    }
    dt <- l_types[[3]]
    dt <- l_types[[2]][dt]
    dt <- l_types[[1]][dt]
    dt <- dt[, .(colName, colClass2015, colClass2016, colClass2017)]
    
    s_1415 <- s_1415[, PHONE:=as.character(PHONE)]
    s_1516 <- s_1516[, PHONE:=as.character(PHONE)]
    s_1617 <- s_1617[, PHONE:=as.character(PHONE)]
    s_1617 <- s_1617[, UG:=as.integer(UG)]
    s_1617 <- s_1617[, UG:=as.integer(UG)]
    s_1617 <- s_1617[, TOTETH:=as.integer(TOTETH)]
    #----------------#
    schools_post2014 <- rbindlist(list(s_1415, s_1516, s_1617), use.names = TRUE, fill=TRUE)
    saveRDS(schools_post2014, file = schools_post2014_location)
  } else {
    schools_post2014 <- readRDS(schools_post2014_location)
  }
  return(schools_post2014)
}
