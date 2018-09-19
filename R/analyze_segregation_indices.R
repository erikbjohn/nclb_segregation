analyze_segregation_indices <- function(){
  analyze_segregation_indices_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/analyze_segregation_indices.rds'
  if(!file.exists(analyze_segregation_indices_location)){
    l_schools <- get_schools()
    l_inds <- create_segregation_indices()
  
    # First, examine overall trends
    base_indices <- l_inds[[1]]
    summary(l_inds[[1]]$dt_indices_base)
    summary(l_inds[[11]]$dt_indices_base)
    summary(l_inds[[11]]$dt_indices_same)
    dissim_same <- sapply(l_inds, function(x) mean(x$dt_indices_same[!is.na(dissimilarity) & !is.infinite(dissimilarity)]$dissimilarity))
    exposure_bw <- sapply(l_inds, function(x) mean(x$dt_indices_same[!is.na(exposure_bw) & !is.infinite(exposure_bw)]$exposure_bw))
    exposure_wb <- sapply(l_inds, function(x) mean(x$dt_indices_same[!is.na(exposure_wb) & !is.infinite(exposure_wb)]$exposure_wb))
    plot(dissim_same)
    plot(exposure_bw)
    plot(exposure_wb)
    
    # Condition on school district release
    # Working....
    dt_releases_schools <- l_schools$dt[!is.na(YEAR.LIFTED)]
    dt_releases_districts <- unique(dt_releases_schools[, .(LEAID, LEAID.1989, YEAR, YEAR.LIFTED, LIFTED)])
    setkey(dt_releases_districts, LEAID, YEAR)
    
    # Use only those that are in all years to ensure consistent sample
    # Base geography
    l_dt_inds <- list()
    l_dt_inds$base <- rbindlist(lapply(l_inds, '[[', 3), use.names = TRUE, fill=TRUE)
    l_dt_inds$base$year_base <- NULL
    l_dt_inds$base$boundaries <- 'consistent'
    
    l_dt_inds$current <- rbindlist(lapply(l_inds, '[[', 4), use.names = TRUE, fill=TRUE)
    l_dt_inds$current$boundaries <- 'contemporary'
    
    dt_inds <- rbindlist(l_dt_inds, use.names=TRUE, fill=TRUE)
    
    # Weird indices
    indices_lea <- unique(dt_inds$shp_LEA)
    schools_lea <- unique(l_schools$dt$LEAID)
    
    indices_lea[which(!(indices_lea %in% schools_lea))]
    dt_inds[shp_LEA %in% indices_lea]
    length(indices_lea[which(!(indices_lea %in% schools_lea))])
    
    schools_lea[which(!(schools_lea %in% indices_lea))]
    length(schools_lea[which(!(schools_lea %in% indices_lea))])
    
    dt_inds <- dt_inds[!is.na(dissimilarity) & !is.na(exposure_bw) & !is.na(exposure_wb)]
    dt_inds <- dt_inds[!is.infinite(dissimilarity) & !is.infinite(exposure_bw) & !is.infinite(exposure_wb)]
    dt_inds <- dt_inds[, year_count:=.N, by=shp_LEA]
    max_years <- max(dt_inds$year_count)
    
    #dt_inds <- dt_inds[year_count==max_years]
    #dt_inds <- dt_inds[year_count > 15]
    
    dt_inds$year_count <- NULL
    setkey(dt_inds, shp_LEA, year)
    
    dt <- dt_releases_districts[dt_inds]
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
    
    l_out$lifted_stats <- (table(unique(dt[, .(LEAID, int_year_lifted, LIFTED)])$LIFTED))
    
    # Plot all districts with contemporary boundary calculations
    l_dts_contemp <- list()
    
    l_dts_contemp$restricted_never <- dt[LEAID %in% districts_restricted_never & boundaries %in% 'contemporary',
                                         .(dissimilarity=mean(dissimilarity),
                                           exposure_bw=mean(exposure_bw), 
                                           exposure_wb=mean(exposure_wb),
                                           sample_type='restricted never'),
                                         by=YEAR]
    
    l_dts_contemp$released_1990 <- dt[LEAID %in% districts_released_1990 & boundaries %in% 'contemporary'][, .(dissimilarity=mean(dissimilarity),
                                                                      exposure_bw=mean(exposure_bw), 
                                                                      exposure_wb=mean(exposure_wb),
                                                                      sample_type='released < 1990'),
                                                                  by=YEAR]
    
    l_dts_contemp$released_1990_2000 <- dt[LEAID %in% districts_released_1990_2000 & boundaries %in% 'contemporary'][, .(dissimilarity=mean(dissimilarity),
                                                                                exposure_bw=mean(exposure_bw), 
                                                                                exposure_wb=mean(exposure_wb),
                                                                                sample_type='released 1990 - 2000'),
                                                                            by=YEAR]
    
    l_dts_contemp$released_2000_2010 <- dt[LEAID %in% districts_released_2000_2010 & boundaries %in% 'contemporary'][, .(dissimilarity=mean(dissimilarity),
                                                                                exposure_bw=mean(exposure_bw), 
                                                                                exposure_wb=mean(exposure_wb),
                                                                                sample_type='released 2000 - 2010'),
                                                                            by=YEAR]
 
      
    dt_plots <- rbindlist(l_dts_contemp, use.names = TRUE, fill=TRUE)  

    l_out$Dissimilarity_contemporary <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=dissimilarity, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Dissimilarity')
    print(l_out$Dissimilarity_contemporary)
    
    l_out$ExposureBW_contemporary <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=exposure_bw, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Exposure BW')
    print(l_out$ExposureBW_contemporary)
    
    l_out$ExposureWB_contemporary <- ggplot2::ggplot(dt_plots, aes(x=YEAR, y=exposure_wb, group=sample_type, color=sample_type)) + 
      geom_line() + 
      ggtitle('Exposure WB')
    print(l_out$ExposureWB_contemporary)
    
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
