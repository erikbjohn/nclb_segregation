compare_secession_sources <- function(){
  # Compares secessions from 2 sources: 
  # 1:  EDGE website: EDGE_mergers_splits.xlsx  and
  # 2:  EDBUILD: schooldistrict_secessions_since2000.xlsx 
  # Source 1: 2005-2018 (2015 effective)
  # Source 2: 2000-2015
  # Source 1 is boundary based, source 2 includes news articles, etc.
  source('R/get_edge_splits.R')
  source('R/get_secessions.R')
  source('R/get_schools.R')
  sch <- get_schools()$dt
  
  src_EDGE <- get_edge_splits()
  src_EDGE <- src_EDGE[, split_count:=.N, by=.(seceded_from_district_ncesid, secession_year)]
  src_EDGE <- src_EDGE[split_count>1]
  src_EDGE <- src_EDGE[seceded_from_district_ncesid != seceding_district_ncesid]
  src_EDBUILD <- get_secessions()
  src_EDBUILD <- src_EDBUILD[secession_status=='Seceded']
  src_EDBUILD <- src_EDBUILD[, seceding_district_ncesid:=stringr::str_remove_all(seceding_district_ncesid, '^0')]
  src_EDBUILD <- src_EDBUILD[, seceded_from_district_ncesid:=as.character(stringr::str_remove_all(seceded_from_district_ncesid, '^0'))]
  setkey(src_EDBUILD, seceding_district_ncesid, secession_year)
  
  dists <- sch[, .SD[1], by=.(LEAID, YEAR), .SDcols=c('LEANM')]
  dists <- dists[, LEAID_dists := LEAID]
  dists <- dists[, LEANM_dists := LEANM]
  setkey(dists, LEAID, YEAR)
  # Subset both of these to only included districts in the schools data set
  
  #src_EDGE analysis (with school demo data)
  setkey(src_EDGE, seceding_district_ncesid, secession_year)
  dists_src_EDGE <- dists[src_EDGE]
  cat('EDGE DATA info ', '\n')
  cat('Total Secessions: ', nrow(src_EDGE), '\n')
  cat('Measured Secessions: ', nrow(dists_src_EDGE[!is.na(LEAID_dists)]), '\n')
  cat('Total Secessions by state and year', '\n')
  table(dists_src_EDGE[, .(state_name, YEAR)])
  cat('Measured Secessions by state and year', '\n')
  table(dists_src_EDGE[!is.na(LEAID_dists),.(state_name,YEAR)])
  cat('Missing Secession school data by state and year', '\n')
  table(dists_src_EDGE[is.na(LEAID_dists),.(state_name,YEAR)])
   
  #src_EDBUILD analysis (with school demo data)
   dists_src_EDBUILD <- dists[src_EDBUILD]
  cat('Stanford info ', '\n')
  cat('Total Secessions: ', nrow(src_EDBUILD), '\n')
  cat('Measured Secessions: ', nrow(dists_src_EDBUILD[!is.na(LEAID_dists)]), '\n')
  cat('Total Secessions by state and year', '\n')
  table(dists_src_EDBUILD[, .(state_name, YEAR)])
  cat('Measured Secessions by state and year', '\n')
  table(dists_src_EDBUILD[!is.na(LEAID_dists),.(state_name,YEAR)])
  cat('Missing Secession school data by state and year', '\n')
  table(dists_src_EDBUILD[is.na(LEAID_dists),.(state_name,YEAR)])
  
  # src_EDGE to src_EDBUILD analysis (for only those with school demo data)
  sub_EDGE <- dists_src_EDGE[!is.na(LEAID_dists)][,.(state_name, seceded_from_district_ncesid, seceding_district_ncesid=LEAID,
                                            seceding_district, secession_year=YEAR, secession_status)]
  sub_EDGE <- sub_EDGE[, seceded_from_district_ncesid := as.character(seceded_from_district_ncesid)]
  sub_EDBUILD <- dists_src_EDBUILD[!is.na(LEAID_dists)][,.(state_name, seceded_from_district_ncesid, seceding_district_ncesid=LEAID,
                                               seceding_district, secession_year=YEAR, secession_status)]
  sub_tot <- rbindlist(list(sub_EDGE, sub_EDBUILD), use.names = TRUE, fill=TRUE)
  sub_tot <- sub_tot[, .SD[1], by=.(seceded_from_district_ncesid, seceding_district_ncesid)]
  
  setkey(sub_EDGE, seceded_from_district_ncesid, seceding_district_ncesid)
  setkey(sub_EDBUILD, seceded_from_district_ncesid, seceding_district_ncesid)
  left_sub2 <-sub_EDGE[sub_EDBUILD]
  left_sub1 <- sub_EDBUILD[sub_EDGE]
  inner_join <- left_sub2[!is.na(state_name)]
  cat('There are', nrow(sub_tot), 'unique secessions between the two datasets')
  cat('There are', nrow(left_sub2), 'secessions found in both datasets (inner join)', '\n')
  cat('There are', nrow(left_sub2[is.na(state_name)]), ' found in the EDBUILD data missing from the EDGE data')
  cat('There are', nrow(left_sub1[is.na(state_name)]), ' found in the EDGE data missing from the EDBUILD data')
  
  # EDBUILD data may be missing from EDGE because of the secession year not showing up in the NCESSCH data.
  # UPDATE THE secession years to reflect
  # 
  
  
  
}
