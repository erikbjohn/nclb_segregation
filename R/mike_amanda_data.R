mike_amanda_data <- function(){
  dt <- fread('~/Dropbox/pkg.data/nclb_segregation/Clean/schools_for_amanda.csv')
  table(dt$seceded)
 # View(dt[stringr::str_detect(SCHNAM, '(?i)Boaz Middle')])
  
  # Need to use NCESSCH_presecession
  dt[, WHITE_SHARE:=(WHITE/(BLACK + WHITE))]
  dt <- dt[!is.na(WHITE_SHARE)]
  
  # Check for high grade and fill in if missing
  elem_schools <- unique(dt[HIGHGRADE %in% c('01', '02', '03', '04', '05', '06')]$NCESSCH)

  sample <- dt[NCESSCH %in% elem_schools]
  sample$post_secession <- 0
  sample <- sample[YEAR >= secession_year, post_secession := 1]
  sample[, .(mean_white=mean(WHITE_SHARE)), by=post_secession]
  
  # New schools
  NCESSCH_first_year <- sample[order(NCESSCH_preSecession, YEAR), .SD[1], by=NCESSCH_preSecession]
  NCESSCH_new_schools <- NCESSCH_first_year[YEAR >= secession_year]
  sample$new_school <- 0
  sample <- sample[NCESSCH %in% NCESSCH_new_schools$NCESSCH, new_school:=1]
  
  sample[, .(mean_white=mean(WHITE_SHARE)), by=.(new_school, post_secession)]
  
  # From districts
  from_districts <- unique(dt[seceded==0, .(LEAID.1989 = as.character(LEAID.1989), secession_year)])
  from_districts <- from_districts[, .SD[1], by=LEAID.1989]
  setkey(from_districts, LEAID.1989)
  seceded_schools <- unique(dt$NCESSCH_preSecession)
  
  source('R/get_schools.R')
  schools <- get_schools()
  schools <- schools$dt
  schools <- schools[LEAID %in% from_districts$LEAID.1989]
  schools <- schools[!(NCESSCH %in% seceded_schools)]
  schools <- schools[!(G07 > 1 | G08 > 1 | G09 > 1 | G10 > 1 | G11 > 1 | G12 > 1)]
  schools <- schools[!(KG > 0 & G01 < 0)]
  schools <- schools[, WHITE_SHARE:=(WHITE/(BLACK + WHITE))]
  schools <- schools[!is.na(WHITE_SHARE)]
  setkey(schools, LEAID.1989)
  schools <- from_districts[schools]
  schools$post_secession <- 0
  schools <- schools [YEAR >= secession_year, post_secession := 1]
  NCESSCH_first_year_school <- schools[order(NCESSCH, YEAR), .SD[1], by=NCESSCH]
  NCESSCH_new_schools_school <- NCESSCH_first_year_school[YEAR >= secession_year]
  schools$new_school <- 0
  schools <- schools[NCESSCH %in% NCESSCH_new_schools_school$NCESSCH, new_school:=1]
  
  dt_stats <- rbindlist(list(sample[, .(YEAR, LEAID.1989, secession_year, BLACK, WHITE, WHITE_SHARE, post_secession,  new_district=1, new_school, NCESSCH =as.character(NCESSCH_preSecession))],
                          schools[,.(YEAR, LEAID.1989, secession_year, BLACK, WHITE, WHITE_SHARE, post_secession, new_district=0, new_school, NCESSCH)]),
                        use.names = TRUE, fill=TRUE)
  dt_stats <- dt_stats[, .SD[1], by=.(YEAR, NCESSCH)]
  dt_stats$interpolated <- 0
  #View(dt_stats[, .(mean_white=mean(WHITE_SHARE)), by=.(new_district, post_secession, new_school)][order(new_district, post_secession)])
  NCESSCHs <- unique(dt_stats$NCESSCH)
  dt_interp_check <- unique(dt_stats[, .(NCESSCH, post_secession)])
  n_interp_checks <- nrow(dt_interp_check)
  dt_impute <- data.table()
  for(i_NCESSCH in 1:n_interp_checks){
    sNCESSCH <- dt_interp_check[i_NCESSCH]$NCESSCH
    intPostSecession <- dt_interp_check[i_NCESSCH]$post_secession
    dt_stat <- dt_stats[NCESSCH==sNCESSCH & post_secession== intPostSecession]
    min_year <- min(dt_stat$YEAR)
    max_year <- max(dt_stat$YEAR)
    years <- min_year:max_year
    missing_years <- years[!years %in% dt_stat$YEAR]
    if(length(missing_years)>0){
      print(paste(i_NCESSCH, paste0(dt_interp_check[i_NCESSCH], collapse=' ')))
      x <- dt_stat$YEAR
      l_y <- list(dt_stat$WHITE, dt_stat$BLACK)
      if(length(x)>3){
        l_mods <- lapply(l_y, function(y) smooth.spline(x, y))
        l_shares <- sapply(l_mods, function(mod) predict(mod, missing_years)$y, simplify=FALSE)
      } else {
        l_shares <- lapply(l_y, function(y) approx(x, y, xout=missing_years)$y)
      }
      imp_WHITE <- floor(l_shares[[1]])
      imp_BLACK <- floor(l_shares[[2]])
      imp_WHITE_SHARE <- imp_WHITE/(imp_WHITE + imp_BLACK)
      dt_imp_year <- data.table(YEAR=missing_years, WHITE = imp_WHITE, BLACK=imp_BLACK, LEAID.1989=dt_stat$LEAID.1989[1], WHITE_SHARE=imp_WHITE_SHARE,
                                secession_year=dt_stat$secession_year[1], 
                                  new_district = dt_stat$new_district[1], new_school = dt_stat$new_school[1],
                                  NCESSCH=sNCESSCH)
      dt_imp_year <- dt_imp_year[,post_secession := 0]
      dt_imp_year <- dt_imp_year[YEAR>=secession_year, post_secession:=1]
      dt_imp_year$interpolated <- 1
      dt_stats <- rbindlist(list(dt_stats, dt_imp_year), use.names = TRUE, fill=TRUE)
    }
   
  }
  
  dt_stats  <- dt_stats[, state:=substr(NCESSCH, 1, 2)]
  dt_stats <- dt_stats[, years_since_secession:=(YEAR - secession_year)]
  dt_stats <- dt_stats[new_school==1, open_school:=min(YEAR), by=NCESSCH]
  dt_stats <- dt_stats[open_school==YEAR, opening_year:=1, by=NCESSCH]  
  dt_stats <- dt_stats[is.na(opening_year), opening_year:=0]
  # New schools
  dt_new_schools <- dt_stats[, .(new_schools_year=sum(new_school)), by=.(years_since_secession, new_district)]
  dt_new_schools <- dt_new_schools[new_district==1, district_type:='Seceeded']
  dt_new_schools <- dt_new_schools[new_district==0, district_type:='Original']
  
  ggplot(dt_new_schools[years_since_secession >=0], aes(x=years_since_secession, y=new_schools_year, color=district_type)) +
    geom_point(aes(shape=district_type), size=4) +
    ylab('Number of New Schools Opened') +
    xlab('Years since secession')
  
  ggsave('/home/ebjohnson5/Dropbox/pkg.data/nclb_segregation/plots/NewElementarySchoolsByYearSinceSecession.png')
  
  # Examine how the white share changes
  dt_stats <- dt_stats[new_district==1, district_type:='Seceeded']
  dt_stats <- dt_stats[new_district==0, district_type:='Original']
  
  na_white_share <- unique(dt_stats[is.na(WHITE_SHARE)]$NCESSCH)
  dt_stats <- dt_stats[!(NCESSCH %in% na_white_share)]
  
  dt_stats <- dt_stats[, max_years_since_secession:=max(years_since_secession), by=NCESSCH]

  
#  schools_seced_year_plus_three <- dt_stats[new_district==1 & years_since_secession==3]$NCESSCH
#  schools_seced_year_plus_four <-dt_stats[new_district==1 & years_since_secession ==4]$NCESSCH
#  ncessch_in_3_not_4 <- schools_seced_year_plus_three[!(schools_seced_year_plus_three %in% schools_seced_year_plus_four)]
#  dt_stats$in_three_not_four <- 0
#  dt_stats <- dt_stats[NCESSCH %in% ncessch_in_3_not_4, in_three_not_four:=1]
#  dt_stats <- dt_stats[in_three_not_four==0]
  
  dt_stats <- dt_stats[max_years_since_secession == 10] 
  
  dt_white_share <- dt_stats[, .(WHITE_SHARE=mean(WHITE_SHARE)), by=.(years_since_secession, district_type)]
  
  district_durations <- dt_stats[order(-YEAR), .SD[1], by=NCESSCH]
  table(district_durations[new_district==1]$years_since_secession)

  ggplot(dt_white_share[years_since_secession<=9 & years_since_secession>-15], aes(x=years_since_secession, y=WHITE_SHARE, color=district_type)) + 
    geom_point()
  
  ggsave('/home/ebjohnson5/Dropbox/pkg.data/nclb_segregation/plots/WhiteSharesByYearSinceSecession_full.png')
  
  
  ## Secession Case studies
 
  # Basic White and Black Stats, number of schools, number of new schools
  dt_stats$school_opened_this_year <- 0
  dt_stats <- dt_stats[open_school==YEAR, school_opened_this_year:=1]
  dt_base_stats <- dt_stats[, .(n_White=sum(WHITE),
                                n_BLACK=sum(BLACK),
                                mean_WHITE_SHARE=mean(WHITE_SHARE),
                                n_schools_total=.N,
                                n_schools_opened=sum(school_opened_this_year)),
                                by=.(LEAID.1989, YEAR)]
  
  lea_secessions <- unique(dt$LEAID.1989)
  source('R/get_secessions.R')
  source('R/functions_segregation_indices.R')
  secessions <- get_secessions()
  dt_segs <- list()
  for(lea_secession in lea_secessions){
    dt_lea <- dt_stats[LEAID.1989==lea_secession]
    secession_info <- secessions[seceded_from_district_ncesid %in% stringr::str_pad(lea_secession, 7, side='left', pad='0')]
    secession_info <- secession_info[order(secession_year)][1]
    years <- unique(dt_lea$YEAR)
    # Segregation Indices stats
    l_seg_indices_before <- lapply(years, function(sYear) 
      functions_segregation_indices(dt_lea[YEAR == sYear], map_year = secession_year, geo_scale = 'LEAID.1989')[1, 6:9])
    dt_seg_indices <- rbindlist(l_seg_indices_before, use.names = TRUE, fill=TRUE)
    dt_seg_indices$YEAR <- years
    dt_seg_indices$LEAID.1989 <- as.character(lea_secession)
    dt_seg_indices$LEANM.1989 <- secession_info[1]$sceded_from_behind_district
    dt_segs <- rbindlist(list(dt_segs, dt_seg_indices), use.names = TRUE, fill=TRUE)
  }
  setkey(dt_segs, LEAID.1989, YEAR)
  setkey(dt_base_stats, LEAID.1989, YEAR)
  dt_base_stats <- dt_segs[dt_base_stats]  
  dt_base_stats <- dt_base_stats[is.na(index_dissimilarity), index_dissimilarity:=0]
  dt_base_stats <- dt_base_stats[is.na(index_exposure_bw), index_exposure_bw:=0]  
  dt_base_stats <- dt_base_stats[is.na(index_finnegan), index_finnegan:=0]
  dt_base_stats <- dt_base_stats[is.na(index_exposure_wb), index_exposure_wb:=0]
  saveRDS(dt_base_stats, file='~/Dropbox/pkg.data/nclb_segregation/Clean/district_secession_stats.rds')
}
