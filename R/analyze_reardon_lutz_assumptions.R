analyze_reardon_lutz_assumptions <- function(){
  schools <- get_schools()$dt
  schools <- schools[!is.na(MEMBER), n_students_district := sum(MEMBER), by = c('YEAR', 'LEAID')]
  schools <- schools[n_students_district > 0]
  setkey(schools, NCESSCH, YEAR)
  
  
  dt_indices <- create_segregation_indices()
  dt_inds <- unique(dt_indices[, .(YEAR, index_geo_year, index_geo_scale, index_geo_id, 
                                   index_dissimilarity, index_exposure_bw, index_exposure_wb, index_finnegan)])
  dt_inds <- dt_inds[,year_count:=.N, by= c('index_geo_year', 'index_geo_scale', 'index_geo_id')]
  max_years <- max(dt_inds$year_count)
  #dt_inds <- dt_inds[year_count==max_years]
  
  # Plot basic pictures
  dt_inds_agg <- dt_inds[, .(YEAR, index_geo_year, index_geo_scale,
                             index_dissimilarity, index_exposure_bw, 
                             index_exposure_wb, index_finnegan)]
  dt_agg <- data.table(tidyr::gather(dt_inds_agg, key=measure, value=value , 4:7))
  dt_agg <- dt_agg[, .(mean_value=mean(value)), by=.(YEAR, index_geo_year, index_geo_scale, measure)]
  setkey(indices, NCESSCH, YEAR)
  schools <- schools[indices]
  schools <- schools[!is.na(index_dissimilarity) & !is.na(index_exposure_bw) &
                       !is.na(index_exposure_wb) & !is.na(index_finnegan)]
  
  schools_full <- schools[, dissimilarity:=mean(index_dissimilarity), by=YEAR]
  schools_full <- unique(schools_full[, .(YEAR, dissimilarity)])
  ggplot2::ggplot(data=schools_full, aes(x=YEAR, y=dissimilarity)) +
    geom_line()
  
  
  indices <- create_segregation_indices()
  
}