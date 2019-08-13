get_state_fips <- function(){
  state_fips_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/state_fips.rds'
  if(!file.exists(state_fips_location)){
    dt_state_fips <- data.table(state.fips)
    dt_state_fips <- dt_state_fips[, fips:=as.character(fips)]
    dt_state_fips <- dt_state_fips[nchar(fips)==1, fips:=paste0(0, fips)]
    dt_state_fips <- dt_state_fips[, .(STFIPS=fips, abb, STATE_NAME=polyname)]
    dt_state_fips <- dt_state_fips[stringr::str_detect(STATE_NAME, '\\:'), STATE_NAME := stringr::str_extract(STATE_NAME, '(?=^).+(?=\\:)')]
    dt_state_fips <- unique(dt_state_fips)
    dt_state_fips <- rbindlist(list(dt_state_fips, data.table(STFIPS='02', abb='AK', STATE_NAME='alaska')))
    dt_state_fips <- rbindlist(list(dt_state_fips, data.table(STFIPS='15', abb='HI', STATE_NAME='hawaii')))
    saveRDS(dt_state_fips, file=state_fips_location)
  } else {
    dt_state_fips <- readRDS(state_fips_location)
  }
  return(dt_state_fips)
}