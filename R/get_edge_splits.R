get_edge_splits <- function(){
  edge_splits_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/edge_splits.rds'
  if(!file.exists(edge_splits_location)){
    dt <- data.table::fread('~/Dropbox/pkg.data/nclb_segregation/SchoolData/EDGE_mergers_splits/splits.csv')
    setnames(dt, names(dt), c('state_abbr', 'seceded_from_district_ncesid', 'sceded_from_behind_district',
                              'secession_school_year',
                              'ncesid_1', 'name_1',
                              'ncesid_2', 'name_2',
                              'ncesid_3', 'name_3',
                              'ncesid_4', 'name_4',
                              'ncesid_5', 'name_5',
                              'ncesid_6', 'name_6',
                              'ncesid_7', 'name_7',
                              'ncesid_8', 'name_8'))
    dt <- dt[, dist_1:=paste0(ncesid_1, 'xxx', name_1)]
    dt <- dt[, dist_2:=paste0(ncesid_2, 'xxx', name_2)]
    dt <- dt[, dist_3:=paste0(ncesid_3, 'xxx', name_3)]
    dt <- dt[, dist_4:=paste0(ncesid_4, 'xxx', name_4)]
    dt <- dt[, dist_5:=paste0(ncesid_5, 'xxx', name_5)]
    dt <- dt[, dist_6:=paste0(ncesid_6, 'xxx', name_6)]
    dt <- dt[, dist_7:=paste0(ncesid_7, 'xxx', name_7)]
    dt <- dt[, dist_8:=paste0(ncesid_8, 'xxx', name_8)]
    
    dt.m1  <- melt(dt, id.varss=c('sceded_from_district_ncesid','st_name', 'sceded_from_behind_district', 'secession_year'),
                   measure.vars=c('dist_1', 
                                  'dist_2',
                                  'dist_3',
                                  'dist_4',
                                  'dist_5',
                                  'dist_6',
                                  'dist_7',
                                  'dist_8'))
    
    dt.m1 <- dt.m1[, .(state_abbr, seceded_from_district_ncesid, secession_school_year, dist = value)]
    dt.m1 <- dt.m1[, seceding_district_ncesid:=stringr::str_extract(dist, '.+(?=xxx)')]  
    dt.m1 <- dt.m1[, seceding_district:=stringr::str_extract(dist, '(?<=xxx).+')]
    dt.m1 <- dt.m1[!is.na(seceding_district_ncesid) & seceding_district!='NA']
    dt.m1$dist <- NULL
    dt.m1 <- dt.m1[, secession_year:=as.numeric(paste0('20',stringr::str_extract(secession_school_year, '(?<=\\-)[0-9]{2,2}$')))]
    dt.m1$secession_school_year <- NULL
    states <- get_state_fips()
    setkey(dt.m1, state_abbr)
    setkey(states, abb)
    dt.m1 <- states[dt.m1]
    setnames(dt.m1, 'STATE_NAME', 'state_name')
    dt.m1 <- dt.m1[, state_name:=stringr::str_to_title(state_name)]
    dt.m1$STFIPS <- NULL
    dt.m1$abb <- NULL
    dt.m1$secession_status <- 'Seceded'  
    #dt.m1 <- dt.m1[, seceding_district_ncesid:=stringr::str_pad(seceding_district_ncesid, 7, side='left', pad='0')]
    #dt.m1 <- dt.m1[, seceded_from_district_ncesid:=stringr::str_pad(seceded_from_district_ncesid, 7, side='left', pad='0')]
    saveRDS(dt.m1, file=edge_splits_location)
  } else {
    dt.m1 <- readRDS(edge_splits_location)
  }
  return(dt.m1)
}