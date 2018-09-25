analyze_segregation_indices <- function(){
  analyze_segregation_indices_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/analyze_segregation_indices.rds'
  if(!file.exists(analyze_segregation_indices_location)){
    l_schools <- get_schools()
    dt_indices <- create_segregation_indices()
  
    # Condition on school district release
    # Working....
    dt_releases_schools <- l_schools$dt[!is.na(YEAR.LIFTED)]
    dt_releases_districts <- unique(dt_releases_schools[, .(LEAID, LEAID.1989, YEAR, YEAR.LIFTED, LIFTED)])
    setkey(dt_releases_districts, LEAID, YEAR)
    
    
    # Drop indices with NA
    dt_indices <- dt_indices[!is.na(index_dissimilarity) & !is.na(index_exposure_bw) & !is.na(index_exposure_wb) & !is.na(index_finnegan)]
    dt_indices <- dt_indices[!is.infinite(index_dissimilarity) & !is.infinite(index_exposure_bw) & !is.infinite(index_exposure_wb)]
    
    # Collapse to spatial aggregation level
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
    ggplot2::ggplot(dt_agg[index_geo_scale=='CNTYFIPS'], aes(x=YEAR, y=mean_value, group=measure, color=measure)) +
      ggplot2::geom_line() +
      ggtitle('School Segregation Measures by County')
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/county_level_segregation_mean.pdf')
    
    ggplot2::ggplot(dt_agg[index_geo_year == 'contemp'], aes(x=YEAR, y=mean_value, group=measure, color=measure)) +
      ggplot2::geom_line() +
      ggtitle('School Segregation Measures: \n Contemporaneous School District Boundary')
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/school_district_segregation_contemporaneous_mean.pdf')
    
    ggplot2::ggplot(dt_agg[index_geo_year == 'base_year'], aes(x=YEAR, y=mean_value, group=measure, color=measure)) +
      ggplot2::geom_line() +
      ggtitle('School Segregation Measures: \n 1989 School District Boundary')
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/school_district_segregation_1989_boundaries_mean.pdf')
    
    
    ## Begin analysis here
    
        #dt_inds <- dt_inds[year_count==max_years]
    #dt_inds <- dt_inds[year_count > 15]
    
    dt_inds$year_count <- NULL
    setkey(dt_inds, index_geo_id, YEAR)
    dt <- dt_releases_districts[dt_inds]
    dt <- dt[!is.na(LEAID)]
    dt <- dt[is.na(YEAR.LIFTED), YEAR.LIFTED:='NEVER RESTRICTED']
    dt <- dt[YEAR.LIFTED %in% 'NEVER RESTRICTED', LIFTED:='NEVER RESTRICTED']
    dt <- dt[!(YEAR.LIFTED %in% c('STILL OPEN', 'NEVER RESTRICTED')), int_year_lifted := as.numeric(YEAR.LIFTED)]
    
       #districts_restricted_ever <- unique(dt[!is.na(int_year_lifted)]$LEAID)
    districts_restricted_never <- unique(dt[YEAR.LIFTED %in% 'NEVER RESTRICTED']$LEAID)
    districts_released_1990 <- unique(dt[!is.na(int_year_lifted) & int_year_lifted < 1990]$LEAID)
    districts_released_1990_2000 <- unique(dt[!is.na(int_year_lifted) & int_year_lifted >= 1990 & int_year_lifted < 2000]$LEAID)
    districts_released_2000_2010 <- unique(dt[!is.na(int_year_lifted) & int_year_lifted >= 2000 & int_year_lifted <= 2010]$LEAID)
    districts_restricted_still_open <- unique(dt[YEAR.LIFTED %in% 'STILL OPEN']$LEAID)


    years <- unique(dt$YEAR)
    dt <- dt[YEAR %in% years]
    
    # Plot restriction lifts by year
    l_out <- list()
    l_out$histogram_year_restriction_lifts <- ggplot(unique(dt[!is.na(int_year_lifted), .(LEAID, int_year_lifted)]), aes(x=int_year_lifted)) +
      geom_histogram(binwidth = 1, color='grey') + 
      theme_bw()
    print(l_out$histogram_year_restriction_lifts)
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/histogramOfReleases.pdf')
    
    l_out$lifted_stats <- (table(unique(dt[, .(LEAID, int_year_lifted, LIFTED)])$LIFTED))
    
    # Plot all districts with contemporary boundary calculations
    l_dts_contemp <- list()
    
    l_dts_contemp$restricted_never <- dt[LEAID %in% districts_restricted_never & index_geo_year %in% 'contemp',
                                         .(dissimilarity=mean(index_dissimilarity),
                                           exposure_bw=mean(index_exposure_bw), 
                                           exposure_wb=mean(index_exposure_wb),
                                           finnegan = mean(index_finnegan),
                                           sample_type='restricted never'),
                                         by=YEAR]
    
    l_dts_contemp$released_1990 <- dt[LEAID %in% districts_released_1990 & index_geo_year %in% 'contemp'][,
                                                                                                          .(dissimilarity=mean(index_dissimilarity),
                                                                                                            exposure_bw=mean(index_exposure_bw), 
                                                                                                            exposure_wb=mean(index_exposure_wb),
                                                                                                            sample_type='released < 1990'),
                                                                                                          by=YEAR]
    
    l_dts_contemp$released_1990_2000 <- dt[LEAID %in% districts_released_1990_2000 & index_geo_year %in% 'contemp'][,
                                                                                                                    .(dissimilarity=mean(index_dissimilarity),
                                                                                                                      exposure_bw=mean(index_exposure_bw), 
                                                                                                                      exposure_wb=mean(index_exposure_wb),
                                                                                                                      sample_type='released 1990 - 2000'),
                                                                                                                    by=YEAR]
    
    l_dts_contemp$released_2000_2010 <- dt[LEAID %in% districts_released_2000_2010 & index_geo_year %in% 'contemp'][, 
                                                                                                                    .(dissimilarity=mean(index_dissimilarity),
                                                                                                                      exposure_bw=mean(index_exposure_bw), 
                                                                                                                      exposure_wb=mean(index_exposure_wb),
                                                                                                                      sample_type='released 2000 - 2010'),
                                                                                                                    by=YEAR]
 
      
    dt_plots <- rbindlist(l_dts_contemp, use.names = TRUE, fill=TRUE)  

    l_out$Dissimilarity_contemporary <- ggplot2::ggplot(dt_plots[as.numeric(YEAR)>1989], aes(x=YEAR, y=dissimilarity, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Dissimilarity - Contemporary Boundaries')
    print(l_out$Dissimilarity_contemporary)
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/Dissimilarity_contemporary_boundaries_byReleaseDate.pdf')
    
    
    l_out$ExposureBW_contemporary <- ggplot2::ggplot(dt_plots[as.numeric(YEAR)>1989], aes(x=YEAR, y=exposure_bw, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Exposure BW')
    print(l_out$ExposureBW_contemporary)
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/ExposureBW_contemporary_boundaries_byReleaseDate.pdf')
    
    
    l_out$ExposureWB_contemporary <- ggplot2::ggplot(dt_plots[as.numeric(YEAR)>1989], aes(x=YEAR, y=exposure_wb, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Exposure WB')
    print(l_out$ExposureWB_contemporary)
    ggsave(file='~/Dropbox/pkg.data/nclb_segregation/plots/ExposureWB_contemporary_boundaries_byReleaseDate.pdf')
    
    # Examine segregation using a consistent base year boundary
    dt_plots <- dt[,.(dissimilarity=mean(dissimilarity),
                      exposure_bw=mean(exposure_bw), 
                      exposure_wb=mean(exposure_wb)),
                      by=.(YEAR, boundaries)]
    
    l_out$boundary_analysis_dissimilarity <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=dissimilarity, group=boundaries, color=boundaries)) + 
      geom_line() + 
      ggtitle('Dissimilarity')
    
    print(l_out$boundary_analysis_dissimilarity)
    
    # Plot for consistent boundaries
    l_dts_consistent <- list()
    
    l_dts_consistent$restricted_never <- dt[LEAID %in% districts_restricted_never & boundaries %in% 'consistent',
                                         .(dissimilarity=mean(dissimilarity),
                                           exposure_bw=mean(exposure_bw), 
                                           exposure_wb=mean(exposure_wb),
                                           sample_type='restricted never'),
                                         by=YEAR]
    
    l_dts_consistent$released_1990 <- dt[LEAID %in% districts_released_1990 & boundaries %in% 'consistent'][, .(dissimilarity=mean(dissimilarity),
                                                                                                               exposure_bw=mean(exposure_bw), 
                                                                                                               exposure_wb=mean(exposure_wb),
                                                                                                               sample_type='released < 1990'),
                                                                                                           by=YEAR]
    
    l_dts_consistent$released_1990_2000 <- dt[LEAID %in% districts_released_1990_2000 & boundaries %in% 'consistent'][, .(dissimilarity=mean(dissimilarity),
                                                                                                                         exposure_bw=mean(exposure_bw), 
                                                                                                                         exposure_wb=mean(exposure_wb),
                                                                                                                         sample_type='released 1990 - 2000'),
                                                                                                                     by=YEAR]
    
    l_dts_consistent$released_2000_2010 <- dt[LEAID %in% districts_released_2000_2010 & boundaries %in% 'consistent'][, .(dissimilarity=mean(dissimilarity),
                                                                                                                         exposure_bw=mean(exposure_bw), 
                                                                                                                         exposure_wb=mean(exposure_wb),
                                                                                                                         sample_type='released 2000 - 2010'),
                                                                                                                     by=YEAR]
    
    
    dt_plots <- rbindlist(l_dts_consistent, use.names = TRUE, fill=TRUE)  
    
    l_out$Dissimilarity_consistent <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=dissimilarity, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Dissimilarity - Consistent')
    print(l_out$Dissimilarity_consistent)
    
    l_out$ExposureBW_consistent <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=exposure_bw, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Exposure BW')
    print(l_out$ExposureBW_consistent)
    
    l_out$ExposureWB_consistent <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=exposure_wb, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Exposure WB')
    print(l_out$ExposureWB_consistent)
    
    # Examine segregation using both consistent and contemporary base year boundary
    dt_plots <- dt[,.(dissimilarity=mean(dissimilarity),
                      exposure_bw=mean(exposure_bw), 
                      exposure_wb=mean(exposure_wb)),
                   by=.(YEAR, boundaries)]
    
    l_out$boundary_analysis_dissimilarity <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=dissimilarity, group=boundaries, color=boundaries)) + 
      geom_line() + 
      ggtitle('Dissimilarity')
    
    print(l_out$boundary_analysis_dissimilarity)
    
    
    dt_plots <- dt[,.(dissimilarity=mean(dissimilarity),
                      exposure_bw=mean(exposure_bw), 
                      exposure_wb=mean(exposure_wb)),
                   by=.(YEAR, boundaries, LIFTED)][, boundaries_lifted:=interaction(boundaries, LIFTED)]
    
    l_out$boundaries_lifted_dissimilarity <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=dissimilarity, group=boundaries_lifted, color=boundaries_lifted)) + 
      geom_line() + 
      ggtitle('Dissimilarity')
    print(l_out$boundaries_lifted_dissimilarity)
    
    l_out$boundaries_lifted_exposure_bw <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=exposure_bw, group=boundaries_lifted, color=boundaries_lifted)) + 
      geom_line() + 
      ggtitle('Exposure_BW')
    print(l_out$boundaries_lifted_exposure_bw)
    
    l_out$boundaries_lifted_exposure_wb <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=exposure_wb, group=boundaries_lifted, color=boundaries_lifted)) + 
      geom_line() + 
      ggtitle('Exposure_WB')
    print(l_out$boundaries_lifted_exposure_wb)
    
    l_out$boundaries_active_dissimilarity <- ggplot2::ggplot(dt_plots[LIFTED %in% 'ACTIVE'], aes(x=YEAR, y=dissimilarity, group=boundaries_lifted, color=boundaries)) + 
      geom_line() + 
      ggtitle('Dissimilarity')
    print(l_out$boundaries_active_dissimilarity)
    
    # Focus on dismissal effect
    dt_dismissed <- dt[LIFTED %in% c('DISMISSED', 'ACTIVE')]
    dt_dismissed <- dt_dismissed[int_year_lifted < 1990, lift_bin := 'Lifted Pre 1990']
    dt_dismissed <- dt_dismissed[int_year_lifted >= 1990, lift_bin := 'Lifted Post 1990']
#    dt_dismissed <- dt_dismissed[int_year_lifted > 2000 & int_year_lifted <= 2010, lift_bin := 'Lifted 2000 - 2010']
    dt_dismissed <- dt_dismissed[LIFTED %in% 'ACTIVE', lift_bin := 'Not Lifted']
    
    dt_plots <- dt_dismissed[,.(dissimilarity=mean(dissimilarity),
                      exposure_bw=mean(exposure_bw), 
                      exposure_wb=mean(exposure_wb)),
                   by=.(YEAR, boundaries, lift_bin)]
    dt_plots <- dt_plots[, boundaries_lift_bin:=interaction(boundaries, lift_bin)]
    
    l_out$boundaries_dismissed_dissimilarity <-
      ggplot2::ggplot(dt_plots, aes(x=YEAR, y=dissimilarity, group=boundaries_lift_bin, color=boundaries_lift_bin)) + 
      geom_line() + 
      ggtitle('Dissimilarity Pre 1990 Dismissal')
    print(l_out$boundaries_dismissed_dissimilarity)
    
    # Next, examine redrawn districts (extensive margin)
    dt_boundary_changes_location <- '~/Dropbox/pkg.data/nclb_segregation/schools_and_districts_years.rds'
    dt_boundary_changes <- readRDS(dt_boundary_changes_location)
    
  
    
    
    saveRDS(l_out, file = analyze_segregation_indices_location)
    
    
  } else {
    l_out <- readRDS(analyze_segregation_indices_location)
  }
  return(l_out)
}
