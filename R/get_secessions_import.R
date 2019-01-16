get_secessions_import <- function(){
  # From website
  get_secessions_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/secessions.rds'
  if(!file.exists(get_secessions_location)){
    schools <- create_segregation_indices()
    raw <- fread('~/Dropbox/pkg.data/nclb_segregation/raw/secession/schooldistrict_secessions_since2000.csv')
    setnames(raw, names(raw), c('state_name', 'secession_status_full', 'seceding_district_ncesid', 
                                'seceding_district', 'seceded_from_district_ncesid', 'sceded_from_behind_district'))
    raw$secession_year <- as.numeric(stringr::str_extract(raw$secession_status_full, stringr::regex('(?<=\\().+(?=\\))')))
    raw$secession_status <- sapply(stringr::str_split(raw$secession_status_full, ' \\('), '[[', 1)
    raw$secession_status_full <- NULL
    raw <- raw[, seceding_district_ncesid := stringr::str_pad(string = seceding_district_ncesid, side = 'l', width = 7, pad='0')]
    raw <- raw[, seceded_from_district_ncesid := stringr::str_pad(string = seceded_from_district_ncesid, side = 'l', width = 7, pad='0')]
    raw$secession_id <- 1:nrow(raw)
    saveRDS(raw, get_secessions_location)
  } else{
    raw <- readRDS(get_secessions_location)
  }
  return(raw)
}
