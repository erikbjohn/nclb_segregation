functions_nclb_sy0506_NCESSCH <- function(dt_clean, schools_06){
  
  # Create State names for multiple and nomatch
  schools_06 <- schools_06[stringr::str_count(schools_06$LEAID) == 7, STFIPS := stringr::str_sub(LEAID, 1,2)]
  schools_06 <- schools_06[stringr::str_count(schools_06$LEAID) == 6, STFIPS := paste0('0',stringr::str_sub(LEAID, 1,1))]
  dt_state_fips <- data.table(state.fips)
  dt_state_fips <- dt_state_fips[, fips:=as.character(fips)]
  dt_state_fips <- dt_state_fips[nchar(fips)==1, fips:=paste0(0, fips)]
  dt_state_fips <- dt_state_fips[, .(STFIPS=fips, STATE_NAME=polyname)]
  dt_state_fips <- dt_state_fips[stringr::str_detect(STATE_NAME, '\\:'), STATE_NAME := stringr::str_extract(STATE_NAME, '(?=^).+(?=\\:)')]
  dt_state_fips <- unique(dt_state_fips)
  dt_state_fips <- rbindlist(list(dt_state_fips, data.table(STFIPS='02', STATE_NAME='alaska')))
  setkey(schools_06, STFIPS)
  setkey(dt_state_fips, STFIPS)
  schools_06 <- dt_state_fips[schools_06]
  vec_ncessch <- vector(mode = 'character', length=nrow(dt_clean))
  # First cut on school name match
  for(i_school in 1:nrow(dt_clean)){
    str_school_name <- dt_clean[i_school]$School_Name
    str_district_name <- dt_clean[i_school]$District_Name
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(', str_school_name, ')'))]$NCESSCH
    if(length(ncessch)==1){
      vec_ncessch[i_school] <- ncessch
    }
    if(length(ncessch)>1){
      vec_ncessch[i_school] <- 'multiple'
    }
    if(length(ncessch)<1){
      vec_ncessch[i_school] <- 'noMatch'
    }
    dt_clean$NCESSCH <- vec_ncessch
  }
  
  # Match Stats
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
  
  # If multiple matches
  for(multiple_ind in multiple_inds){
    str_school_name <- dt_clean[multiple_ind]$School_Name
    str_district_name <- dt_clean[multiple_ind]$District_Name
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(', str_school_name, ')')) &
                            stringr::str_detect(LEANM, paste0('(?i)(', str_district_name, ')'))]$NCESSCH
    if(length(ncessch)==1){
      dt_clean[multiple_ind]$NCESSCH <- ncessch
    }
    if(length(ncessch)>1){
      dt_clean[multiple_ind]$NCESSCH <- 'multiple'
    }
    if(length(ncessch)<1){
      dt_clean[multiple_ind]$NCESSCH <- 'noMatch'
    }
  }
  
  # Match stats
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  
  
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
  # If still multiple matches
  for(multiple_ind in multiple_inds){
    str_school_name <- dt_clean[multiple_ind]$School_Name
    str_district_name <- dt_clean[multiple_ind]$District_Name
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(', str_school_name, ')')) &
                            stringr::str_detect(LEANM, paste0('(?i)(', str_district_name, ')'))]$NCESSCH
    if(length(ncessch)==1){
      dt_clean[multiple_ind]$NCESSCH <- ncessch
    }
    if(length(ncessch)>1){
      dt_clean[multiple_ind]$NCESSCH <- 'multiple'
    }
    if(length(ncessch)<1){
      dt_clean[multiple_ind]$NCESSCH <- 'noMatch'
    }
  }
  # matches pcts
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
  
  # If still multiple matches include state name
  for(multiple_ind in multiple_inds){
    str_school_name <- dt_clean[multiple_ind]$School_Name
    str_district_name <- dt_clean[multiple_ind]$District_Name
    str_state_name <- dt_clean[multiple_ind]$State_Name
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(', str_school_name, ')')) &
                            stringr::str_detect(LEANM, paste0('(?i)(', str_district_name, ')')) &
                            stringr::str_detect(STATE_NAME, paste0('(?i)(', str_state_name, ')'))]$NCESSCH
    if(length(ncessch)==1){
      dt_clean[multiple_ind]$NCESSCH <- ncessch
    }
    if(length(ncessch)>1){
      dt_clean[multiple_ind]$NCESSCH <- 'multiple'
    }
    if(length(ncessch)<1){
      dt_clean[multiple_ind]$NCESSCH <- 'noMatch'
    }
  }
  # matches pcts
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
  # If still multiple matches include force name to be exact (no leading or lagging words)
  # If still multiple matches jsut take the first NCESSCH (only 2 records)
  for(multiple_ind in multiple_inds){
    str_school_name <- dt_clean[multiple_ind]$School_Name
    str_district_name <- dt_clean[multiple_ind]$District_Name
    str_state_name <- dt_clean[multiple_ind]$State_Name
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(?=^)(', str_school_name, ')(?=$)')) &
                            stringr::str_detect(LEANM, paste0('(?i)(', str_district_name, ')')) &
                            stringr::str_detect(STATE_NAME, paste0('(?i)(', str_state_name, ')'))]$NCESSCH[1]
    if(length(ncessch)==1){
      dt_clean[multiple_ind]$NCESSCH <- ncessch
    }
    if(length(ncessch)>1){
      dt_clean[multiple_ind]$NCESSCH <- 'multiple'
    }
    if(length(ncessch)<1){
      dt_clean[multiple_ind]$NCESSCH <- 'noMatch'
    }
  }
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
 
  multiple_inds <-which(dt_clean$NCESSCH %in%  c('noMatch', 'multiple'))
  
  for(multiple_ind in multiple_inds){
    str_school_name <- dt_clean[no_ind]$School_Name
    str_school_name_first_word <- stringr::str_split(str_school_name, ' ')[[1]][1]
    str_district_name <- dt_clean[no_ind]$District_Name
    str_district_name_first_word <- stringr::str_split(str_district_name, ' ')[[1]][1]
    str_state_name <- dt_clean[no_ind]$State_Name
    
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(?=^)(', str_school_name_first_word, ')'))  &
                            stringr::str_detect(LEANM, paste0('(?i)(', str_district_name_first_word, ')')) &
                            stringr::str_detect(STATE_NAME, paste0('(?i)(', str_state_name, ')'))]$NCESSCH
    
    if(length(ncessch)==1){
      dt_clean[no_ind]$NCESSCH <- ncessch
    }
    if(length(ncessch)>1){
      dt_clean[no_ind]$NCESSCH <- 'multiple'
    }
    if(length(ncessch)<1){
      dt_clean[no_ind]$NCESSCH <- 'noMatch'
    }
  }
  
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
  no_inds <-which(dt_clean$NCESSCH == 'noMatch')
  for(no_ind in no_inds){
    str_school_name <- dt_clean[no_ind]$School_Name
    # str_district_name <- dt_clean[no_ind]$District_Name
    str_state_name <- dt_clean[no_ind]$State_Name
    
    ncessch <- schools_06[stringr::str_detect(SCHNAM, paste0('(?i)(?=^)(', str_school_name, ')(?=$)'))  &
                            stringr::str_detect(STATE_NAME, paste0('(?i)(', str_state_name, ')'))]$NCESSCH
    
    if(length(ncessch)==1){
      dt_clean[no_ind]$NCESSCH <- ncessch
    }
    if(length(ncessch)>1){
      dt_clean[no_ind]$NCESSCH <- 'multiple'
    }
    if(length(ncessch)<1){
      dt_clean[no_ind]$NCESSCH <- 'noMatch'
    }
  }
  nrow(dt_clean[!(NCESSCH %in% c('noMatch', 'multiple'))])
  nrow(dt_clean[NCESSCH=='multiple'])
  nrow(dt_clean[NCESSCH=='noMatch'])
  multiple_inds <- which(dt_clean$NCESSCH == 'multiple')
  
  
}