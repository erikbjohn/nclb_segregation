analyze_secessions <- function(){
  seg_indices <- create_segregation_indices()
  seg_indices <- unique(seg_indices[index_geo_year == 'base_year',LEAID:=index_geo_id])
  secessions <- get_secessions()
  dt <- unique(secessions[, .(LEAID=seceded_from_district_ncesid, YEAR = secession_year)])
  dt <- dt[order(LEAID, YEAR)]
  dt <- dt[, YEAR:=head(YEAR,1), by=LEAID]
  dt <- unique(dt[, .(LEAID, secession_year = YEAR)])
  dt$secession_data <- 'secession_data'
  setkey(dt, LEAID)
  setkey(seg_indices, LEAID)  
  seg_indices <- dt[seg_indices]
  seg_indices <- seg_indices[!is.na(secession_data)]
  seg_indices <- seg_indices[!is.na(secession_year)]
  gath <- as.data.table(tidyr::gather(seg_indices, key=measure, value=value, 9:12))
  gath <- gath[, year_diff:=YEAR-secession_year]
  gath <- gath[!is.na(value)]
 # gath <- gath[year_diff > -8 & year_diff < 10]
 
  
 
  
  # Do difference plot
  mean_year_measures <- gath[, .(value=mean(value)), by=.(measure, year_diff)]
  mean_year_measures <- mean_year_measures[!(measure %in% 'index_finnegan')]
  setkey(mean_year_measures, measure, year_diff)
  mean_year_measures <- mean_year_measures[, lag_value := shift(value, 1 , type='lag'), by=measure]
  mean_year_measures <- mean_year_measures[!(is.na(lag_value))]
  mean_year_measures <- mean_year_measures[, value_change:=(value-lag_value)/lag_value]
  mean_year_measures <- mean_year_measures[year_diff < 12 & year_diff > -18]
  mean_year_measures <- mean_year_measures[as.character(year_diff) != '-3']
  
  ggplot2::ggplot(mean_year_measures, aes(x=year_diff, y=value, group=measure, colour=measure)) +
    geom_line() +
    ggtitle('Index levels from secession time = 0')
  ggsave('~/Dropbox/pkg.data/nclb_segregation/plots/Dissimilarity_secession_Base_year_boundaries.pdf')  
  
  ggplot2::ggplot(mean_year_measures, aes(x=year_diff, y=value_change, group=measure, colour=measure)) +
    geom_line() +
    ggtitle('Index levels from secession time = 0')
  
}