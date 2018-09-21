export_data <- function(){
  l_schools <- get_schools()
  l_schools$dt <- l_schools$dt[, .(LEAID, NCESSCH, WHITE, BLACK, YEAR, YEAR.LIFTED, YEAR)]
  setkey(l_schools$dt, LEAID, YEAR)
  # Need to bring forward map districts for these (by year) (shp_LEA)
  
  # School Data
  l_inds <- create_segregation_indices() 
  l_dt_inds$current <- rbindlist(lapply(l_inds, '[[', 4), use.names = TRUE, fill=TRUE)
  l_dt_inds$current$boundaries <- 'contemporary'
  dt_inds_district <- rbindlist(l_dt_inds, use.names=TRUE, fill=TRUE)
  dt_inds_district <- dt_inds_district[boundaries=='contemporary']
  setnames(dt_inds_district,
           c('shp_LEA', 'dissimilarity', 'exposure_bw', 'exposure_wb'),
           c('LEAID', 'dissimilarity_district', 'exposure_bw_district', 'exposure_wb_district'))
  setkey(dt_inds_district, LEAID, year)
  dt_inds_district <- dt_inds_district[, count:=.N, by=c('LEAID', 'year')]
  
  dt_export <- dt_inds_district[l_schools$dt]
    
}