get_nclb <- function(){
  # Scrape, import and clean 2003-2015 nclb school level data
  nclb_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/nclb.rds'
  if(!file.exists(nclb_location)){
    schools <- get_schools()
    l_nclb <- scrape_nclb() 
    
    # Clean up the files to join to ejData
    # Need common column names, school id field, etc.
    dt_files <- data.table()
    
    # Build data names
    for(i_file in 1:length(l_nclb)){
      dt_file <- data.table(cols = names(l_nclb[[i_file]]))
      dt_file$cols_example <- unlist(l_nclb[[i_file]][1000,])
      dt_file$f_name <- names(l_nclb)[i_file]
      dt_files <- rbindlist(list(dt_files, dt_file), use.names = TRUE, fill=TRUE)
    }
    dt_files <- dt_files[, N:=.N, by=cols]
    dt_files <- tidyr::spread(dt_files, f_name, cols_example, fill='')
    dt_files <- dt_files[order(N, decreasing = TRUE)]
    View(dt_files)
    

    # Start with fixing/assigning the ncessch codes
    for(i_file in 1:length(l_nclb)){
      f_year <- names(l_nclb)[i_file]
      cat(f_year)
      dt_clean <- l_nclb[[i_file]]
      if(f_year %in% c('sy0506')){
        dt_clean <- dt_clean[!(State_Name == '')]
        # First join to NCESSCH since only have School name not code
        dt_clean$NCESSCH <- as.character()
        schools_06 <- schools$dt[YEAR %in% '2006']
        dt_clean <- functions_nclb_sy0506_NCESSCH(dt_clean, schools_06)
        
      if(f_year %in% c('sy1516s'){
        dt_clean <- dt_clean[,NCESSCH:=as.character(School_NCES_ID_Code)]
        dt_clean <- dt_clean[,LEAID:=as.character(LEA_NCES_ID)]
        table(dt_clean[, .(State, School_Improvement_Status)])
        
        table(dt_clean[, .(State, School_Title_I_School_Status)])
      }
    }
    
    saveRDS(nclb, nclb_location)
  } else {
    nclb <- readRDS(nclb_location)
  }
}