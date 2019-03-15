scrape_nclb <- function(){
  
  l_nclb_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/l_nclb.rds'
  
  if(!file.exists(l_nclb_location)){
  
    
    if(!file.exists('~/Dropbox/pkg.data/nclb_segregation/raw/download_files_list.rds')){
      abbrevs <- c(stringr::str_to_lower(state.abb),'dc')
      abbrevs <- abbrevs[!abbrevs %in% 'mo']
      download_files <- list()
      download_files$sy0506 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy05-06/',
                                      abbrevs, 's.pdf')
      download_files$sy0607 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy06-07part2/',
                                      abbrevs, 's.pdf')
      download_files$sy0708 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy07-08part1/',
                                      abbrevs, 's.pdf')
      download_files$sy0809 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy08-09part1/',
                                      abbrevs, 's.pdf')
      download_files$sy0910 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy09-10part1/',
                                      abbrevs, 's.pdf')
      # IN 11-12 move to more pdf types
      download_files$sy1112s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                       abbrevs, 's.pdf')
      download_files$sy1112s <- download_files$sy1112s[which(sapply(download_files$sy1112s, url.exists))]
      
      download_files$sy1112pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'pf.pdf')
      download_files$sy1112pf <- download_files$sy1112pf[which(sapply(download_files$sy1112pf, url.exists))]
      
      download_files$sy1112rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'rs.pdf')
      download_files$sy1112rs <- download_files$sy1112rs[which(sapply(download_files$sy1112rs, url.exists))]
      
      download_files$sy1112ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                          abbrevs, 'ois.pdf')
      download_files$sy1112ois <- download_files$sy1112ois[which(sapply(download_files$sy1112ois, url.exists))]
      
      # IN 12-13
      download_files$sy1213s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                       abbrevs, 's.pdf')
      download_files$sy1213s <- download_files$sy1213s[which(sapply(download_files$sy1213s, url.exists))]
      
      download_files$sy1213pf <-  paste~0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                          abbrevs, 'pf.pdf')
      download_files$sy1213pf <- download_files$sy1213pf[which(sapply(download_files$sy1213pf, url.exists))]
      
      download_files$sy1213rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'rs.pdf')
      download_files$sy1213rs <- download_files$sy1213rs[which(sapply(download_files$sy1213rs, url.exists))]
      
      download_files$sy1213ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                          abbrevs, 'ois.pdf')
      download_files$sy1213ois <- download_files$sy1213ois[which(sapply(download_files$sy1213ois, url.exists))]
      
      # 13-14
      download_files$sy1314s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                       abbrevs, 's.pdf')
      download_files$sy1314s <- download_files$sy1314s[which(sapply(download_files$sy1314s, url.exists))]
      
      download_files$sy1314pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'pf.pdf')
      download_files$sy1314pf <- download_files$sy1314pf[which(sapply(download_files$sy1314pf, url.exists))]
      
      download_files$sy1314rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'rs.pdf')
      download_files$sy1314rs <- download_files$sy1314rs[which(sapply(download_files$sy1314rs, url.exists))]
      
      download_files$sy1314ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'ois.pdf')
      download_files$sy1314ois <- download_files$sy1314ois[which(sapply(download_files$sy1314ois, url.exists))]
      
      # 14-15
      download_files$sy1415s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                       abbrevs, 's.pdf')
      download_files$sy1415s <- download_files$sy1415s[which(sapply(download_files$sy1415s, url.exists))]
      
      download_files$sy1415pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'pf.pdf')
      download_files$sy1415pf <- download_files$sy1415pf[which(sapply(download_files$sy1415pf, url.exists))]
      
      download_files$sy1415rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'rs.pdf')
      download_files$sy1415rs <- download_files$sy1415rs[which(sapply(download_files$sy1415rs, url.exists))]
      
      download_files$sy1415ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'ois.pdf')
      download_files$sy1415ois <- download_files$sy1415ois[which(sapply(download_files$sy1415ois, url.exists))]
      
      # 15-16
      download_files$sy1516s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                       abbrevs, 's.pdf')
      download_files$sy1516s <- download_files$sy1516s[which(sapply(download_files$sy1516s, url.exists))]
      
      download_files$sy1516pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'pf.pdf')
      download_files$sy1516pf <- download_files$sy1516pf[which(sapply(download_files$sy1516pf, url.exists))]
      
      download_files$sy1516rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                         abbrevs, 'rs.pdf')
      download_files$sy1516rs <- download_files$sy1516rs[which(sapply(download_files$sy1516rs, url.exists))]
      
      download_files$sy1516ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                          abbrevs, 'ois.pdf')
      download_files$sy1516ois <- download_files$sy1516ois[which(sapply(download_files$sy1516ois, url.exists))]
      saveRDS(download_files, 'NCLB/download_files_list.rds')
    } else {
      download_files <- readRDS('NCLB/download_files_list.rds')
    }
    mapply(paste, x= names(download_files), y=sapply(download_files, length))
    
    for(i_download_file in 1:length(download_files)){
      download_file <- download_files[[i_download_file]]
      years <- names(download_files[i_download_file])
      if(!dir.exists(paste0('NCLB/Clean/',years))){
        dir.create(paste0('NCLB/Clean/',years))
      }
      if(!dir.exists(paste0('NCLB/Raw/',years))){
        dir.create(paste0('NCLB/Raw/',years))
      }
      rds_files <- paste0('NCLB/Clean/', years, '/', abbrevs, '.rds')
      pdf_files <- paste0('NCLB/Raw/',years,'/', abbrevs, '.pdf')
      year_file_out <- paste0('NCLB/Clean/', names(download_files[i_download_file]), '.rds')
      
      if(!file.exists(year_file_out)){
        dt_years_states <- data.table(pdf=pdf_files, rds = rds_files, download=download_file)
        dt_years_states <- dt_years_states[pdf != 'NCLB/Raw/0910/wy.pdf']
        l_states <- list()
        for(i_year_state in 1:nrow(dt_years_states)){
          state_download_file <- dt_years_states[i_year_state]$download
          cat(state_download_file, sep='\n')
          pdf_file <- dt_years_states[i_year_state]$pdf
          rds_file <- dt_years_states[i_year_state]$rds
          if(!file.exists(pdf_file)){
            download.file(state_download_file, destfile = pdf_file)
          }
          if(!file.exists(rds_file)){
            out1 <- extract_tables(pdf_file, method = 'lattice')
            # Weird state exceptions
            manual_files <- c("NCLB/Raw/ne_05_06.pdf")
            if (pdf_file %in% manual_files){
              if(pdf_file =="NCLB/Raw/ne_05_06.pdf"){
                dt <- data.table('Nebraska', '2005-06', 'Lexington Public Schools', 'Sandoz Elementary',
                                 '', '', '', '', '', '', 'Restructuring Planning')
                dt_tempE <- readRDS('NCLB/Clean/05_06/al.rds')
                setnames(dt, names(dt), names(dt_temp))
              }
            } else { # Normal import
              l.data.table <- lapply(out1, as.data.table)
              if (pdf_file=="NCLB/Raw/0910/fl.pdf"){
                dt_fix <- l.data.table[[26]]
                dt_fix <- dt_fix[7:9]
                dt_fix <- dt_fix[V3=='7510029', V12:='Whole School Intensive Improvement: Year 4']
                dt_fix <- dt_fix[V3=='7510028', V12:='Whole School Improvement: Achieving Year 2']
                dt_fix <- dt_fix[c(1,3)]
                setnames(dt_fix, names(dt_fix), paste0('V',2:13))
                dt_fix <- data.table(V1='', dt_fix)
                l.data.table[[28]] <- dt_fix
              }
              dt <- rbindlist(l.data.table)
              dt_names <- unlist(dt[1])
              dt_names <- stringr::str_replace_all(dt_names, '\\r| |\\/|\\(|\\)', '_')
              dt_names <- stringr::str_replace_all(dt_names, '[\\_]{1,}', '_')
              dt_names <- stringr::str_replace_all(dt_names, '_$', '')
              dt_names <- stringr::str_replace(dt_names, 'Improvem_ent', 'Improvement')
              dt_names <- stringr::str_replace(dt_names, '-_', '-')
              dt_names <- stringr::str_replace(dt_names, 'Missed_Graduatio_n_Rate_HS', 'Missed_Graduation_Rate_HS')
              dt_names <- stringr::str_replace(dt_names, 'Participati_on', 'Participation')
              setnames(dt, names(dt), dt_names)
              school_names <- which(table(dt$School_Name)==max(table(dt$School_Name)))
              school_names <- names(school_names[nchar(names(school_names))>0])
              dt <- dt[School_Name != school_names]
            }
            saveRDS(dt, rds_file)
          } else {
            dt <- readRDS(rds_file)
          }
          l_states[[i_year_state]] <- dt
        }scrape_nclb <- function(){
          
          l_nclb_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/l_nclb.rds'
          
          if(!file.exists(l_nclb_location)){
            
            
            if(!file.exists('~/Dropbox/pkg.data/nclb_segregation/raw/download_files_list.rds')){
              abbrevs <- c(stringr::str_to_lower(state.abb),'dc')
              abbrevs <- abbrevs[!abbrevs %in% 'mo']
              download_files <- list()
              download_files$sy0506 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy05-06/',
                                              abbrevs, 's.pdf')
              download_files$sy0607 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy06-07part2/',
                                              abbrevs, 's.pdf')
              download_files$sy0708 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy07-08part1/',
                                              abbrevs, 's.pdf')
              download_files$sy0809 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy08-09part1/',
                                              abbrevs, 's.pdf')
              download_files$sy0910 <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy09-10part1/',
                                              abbrevs, 's.pdf')
              # IN 11-12 move to more pdf types
              download_files$sy1112s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                               abbrevs, 's.pdf')
              download_files$sy1112s <- download_files$sy1112s[which(sapply(download_files$sy1112s, url.exists))]
              
              download_files$sy1112pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'pf.pdf')
              download_files$sy1112pf <- download_files$sy1112pf[which(sapply(download_files$sy1112pf, url.exists))]
              
              download_files$sy1112rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'rs.pdf')
              download_files$sy1112rs <- download_files$sy1112rs[which(sapply(download_files$sy1112rs, url.exists))]
              
              download_files$sy1112ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                  abbrevs, 'ois.pdf')
              download_files$sy1112ois <- download_files$sy1112ois[which(sapply(download_files$sy1112ois, url.exists))]
              
              # IN 12-13
              download_files$sy1213s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                               abbrevs, 's.pdf')
              download_files$sy1213s <- download_files$sy1213s[which(sapply(download_files$sy1213s, url.exists))]
              
              download_files$sy1213pf <-  paste~0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                  abbrevs, 'pf.pdf')
              download_files$sy1213pf <- download_files$sy1213pf[which(sapply(download_files$sy1213pf, url.exists))]
              
              download_files$sy1213rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'rs.pdf')
              download_files$sy1213rs <- download_files$sy1213rs[which(sapply(download_files$sy1213rs, url.exists))]
              
              download_files$sy1213ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                  abbrevs, 'ois.pdf')
              download_files$sy1213ois <- download_files$sy1213ois[which(sapply(download_files$sy1213ois, url.exists))]
              
              # 13-14
              download_files$sy1314s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                               abbrevs, 's.pdf')
              download_files$sy1314s <- download_files$sy1314s[which(sapply(download_files$sy1314s, url.exists))]
              
              download_files$sy1314pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'pf.pdf')
              download_files$sy1314pf <- download_files$sy1314pf[which(sapply(download_files$sy1314pf, url.exists))]
              
              download_files$sy1314rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'rs.pdf')
              download_files$sy1314rs <- download_files$sy1314rs[which(sapply(download_files$sy1314rs, url.exists))]
              
              download_files$sy1314ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                  abbrevs, 'ois.pdf')
              download_files$sy1314ois <- download_files$sy1314ois[which(sapply(download_files$sy1314ois, url.exists))]
              
              # 14-15
              download_files$sy1415s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                               abbrevs, 's.pdf')
              download_files$sy1415s <- download_files$sy1415s[which(sapply(download_files$sy1415s, url.exists))]
              
              download_files$sy1415pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'pf.pdf')
              download_files$sy1415pf <- download_files$sy1415pf[which(sapply(download_files$sy1415pf, url.exists))]
              
              download_files$sy1415rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'rs.pdf')
              download_files$sy1415rs <- download_files$sy1415rs[which(sapply(download_files$sy1415rs, url.exists))]
              
              download_files$sy1415ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                  abbrevs, 'ois.pdf')
              download_files$sy1415ois <- download_files$sy1415ois[which(sapply(download_files$sy1415ois, url.exists))]
              
              # 15-16
              download_files$sy1516s <- paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                               abbrevs, 's.pdf')
              download_files$sy1516s <- download_files$sy1516s[which(sapply(download_files$sy1516s, url.exists))]
              
              download_files$sy1516pf <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'pf.pdf')
              download_files$sy1516pf <- download_files$sy1516pf[which(sapply(download_files$sy1516pf, url.exists))]
              
              download_files$sy1516rs <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                 abbrevs, 'rs.pdf')
              download_files$sy1516rs <- download_files$sy1516rs[which(sapply(download_files$sy1516rs, url.exists))]
              
              download_files$sy1516ois <-  paste0('https://www2.ed.gov/admins/lead/account/consolidated/sy11-12part2/',
                                                  abbrevs, 'ois.pdf')
              download_files$sy1516ois <- download_files$sy1516ois[which(sapply(download_files$sy1516ois, url.exists))]
              saveRDS(download_files, 'NCLB/download_files_list.rds')
            } else {
              download_files <- readRDS('NCLB/download_files_list.rds')
            }
            mapply(paste, x= names(download_files), y=sapply(download_files, length))
            
            for(i_download_file in 1:length(download_files)){
              download_file <- download_files[[i_download_file]]
              years <- names(download_files[i_download_file])
              if(!dir.exists(paste0('NCLB/Clean/',years))){
                dir.create(paste0('NCLB/Clean/',years))
              }
              if(!dir.exists(paste0('NCLB/Raw/',years))){
                dir.create(paste0('NCLB/Raw/',years))
              }
              rds_files <- paste0('NCLB/Clean/', years, '/', abbrevs, '.rds')
              pdf_files <- paste0('NCLB/Raw/',years,'/', abbrevs, '.pdf')
              year_file_out <- paste0('NCLB/Clean/', names(download_files[i_download_file]), '.rds')
              
              if(!file.exists(year_file_out)){
                dt_years_states <- data.table(pdf=pdf_files, rds = rds_files, download=download_file)
                dt_years_states <- dt_years_states[pdf != 'NCLB/Raw/0910/wy.pdf']
                l_states <- list()
                for(i_year_state in 1:nrow(dt_years_states)){
                  state_download_file <- dt_years_states[i_year_state]$download
                  cat(state_download_file, sep='\n')
                  pdf_file <- dt_years_states[i_year_state]$pdf
                  rds_file <- dt_years_states[i_year_state]$rds
                  if(!file.exists(pdf_file)){
                    download.file(state_download_file, destfile = pdf_file)
                  }
                  if(!file.exists(rds_file)){
                    out1 <- extract_tables(pdf_file, method = 'lattice')
                    # Weird state exceptions
                    manual_files <- c("NCLB/Raw/ne_05_06.pdf")
                    if (pdf_file %in% manual_files){
                      if(pdf_file =="NCLB/Raw/ne_05_06.pdf"){
                        dt <- data.table('Nebraska', '2005-06', 'Lexington Public Schools', 'Sandoz Elementary',
                                         '', '', '', '', '', '', 'Restructuring Planning')
                        dt_tempE <- readRDS('NCLB/Clean/05_06/al.rds')
                        setnames(dt, names(dt), names(dt_temp))
                      }
                    } else { # Normal import
                      l.data.table <- lapply(out1, as.data.table)
                      if (pdf_file=="NCLB/Raw/0910/fl.pdf"){
                        dt_fix <- l.data.table[[26]]
                        dt_fix <- dt_fix[7:9]
                        dt_fix <- dt_fix[V3=='7510029', V12:='Whole School Intensive Improvement: Year 4']
                        dt_fix <- dt_fix[V3=='7510028', V12:='Whole School Improvement: Achieving Year 2']
                        dt_fix <- dt_fix[c(1,3)]
                        setnames(dt_fix, names(dt_fix), paste0('V',2:13))
                        dt_fix <- data.table(V1='', dt_fix)
                        l.data.table[[28]] <- dt_fix
                      }
                      dt <- rbindlist(l.data.table)
                      dt_names <- unlist(dt[1])
                      dt_names <- stringr::str_replace_all(dt_names, '\\r| |\\/|\\(|\\)', '_')
                      dt_names <- stringr::str_replace_all(dt_names, '[\\_]{1,}', '_')
                      dt_names <- stringr::str_replace_all(dt_names, '_$', '')
                      dt_names <- stringr::str_replace(dt_names, 'Improvem_ent', 'Improvement')
                      dt_names <- stringr::str_replace(dt_names, '-_', '-')
                      dt_names <- stringr::str_replace(dt_names, 'Missed_Graduatio_n_Rate_HS', 'Missed_Graduation_Rate_HS')
                      dt_names <- stringr::str_replace(dt_names, 'Participati_on', 'Participation')
                      setnames(dt, names(dt), dt_names)
                      school_names <- which(table(dt$School_Name)==max(table(dt$School_Name)))
                      school_names <- names(school_names[nchar(names(school_names))>0])
                      dt <- dt[School_Name != school_names]
                    }
                    saveRDS(dt, rds_file)
                  } else {
                    dt <- readRDS(rds_file)
                  }
                  l_states[[i_year_state]] <- dt
                }
                # Bind the states into one file
                dt_states <- rbindlist(l_states, use.names = TRUE, fill=TRUE)
                saveRDS(dt_states, file = year_file_out)
              }
            }
            # Load in and pass data back
            rdsFiles <- list.files('NCLB/Clean', pattern = 'sy.+rds',full.names = TRUE)
            l <- lapply(rdsFiles, readRDS)  
            rdsFileNames <- stringr::str_extract(rdsFiles, 'sy.+(?=\\.rds)')
            names(l) <- rdsFileNames
            
            # Bring in 0304, 0405 data from 
            # Next, import 0304, 0405 data
            # From https://www8.gsb.columbia.edu/nclb/overview
            
            sy_0304_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/sy0304.rds'
            if (!file.exists(sy_0304_location)){
              sy_0304 <- readstata13::read.dta13('~/Dropbox/pkg.data/nclb_segregation/raw/national_2003.dta')
              sy_0304 <- data.table(sy_0304)
              saveRDS(sy_0304, file=sy_0304_location)
            } else {
              l$sy0304 <- readRDS(sy_0304_location)
            }
            sy_0405_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/sy0405.rds'
            if (!file.exists(sy_0405_location)){
              sy_0405 <- readstata13::read.dta13('~/Dropbox/Pkg Data/NCLB/national_2004.dta')
              sy_0405 <- data.table(sy_0405)
              saveRDS(sy_0405, file=sy_0405_location)
            } else {
              l$sy0405 <- readRDS(sy_0405_location)
            }
            saveRDS(l, file=l_nclb
        # Bind the states into one file
        dt_states <- rbindlist(l_states, use.names = TRUE, fill=TRUE)
        saveRDS(dt_states, file = year_file_out)
      }
    }
    # Load in and pass data back
    rdsFiles <- list.files('NCLB/Clean', pattern = 'sy.+rds',full.names = TRUE)
    l <- lapply(rdsFiles, readRDS)  
    rdsFileNames <- stringr::str_extract(rdsFiles, 'sy.+(?=\\.rds)')
    names(l) <- rdsFileNames
   
     # Bring in 0304, 0405 data from 
    # Next, import 0304, 0405 data
    # From https://www8.gsb.columbia.edu/nclb/overview
    
    sy_0304_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/sy0304.rds'
    if (!file.exists(sy_0304_location)){
      sy_0304 <- readstata13::read.dta13('~/Dropbox/pkg.data/nclb_segregation/raw/national_2003.dta')
      sy_0304 <- data.table(sy_0304)
      saveRDS(sy_0304, file=sy_0304_location)
    } else {
      l$sy0304 <- readRDS(sy_0304_location)
    }
    sy_0405_location <- '~/Dropbox/pkg.data/nclb_segregation/Clean/sy0405.rds'
    if (!file.exists(sy_0405_location)){
      sy_0405 <- readstata13::read.dta13('~/Dropbox/Pkg Data/NCLB/national_2004.dta')
      sy_0405 <- data.table(sy_0405)
      saveRDS(sy_0405, file=sy_0405_location)
    } else {
      l$sy0405 <- readRDS(sy_0405_location)
    }
    saveRDS(l, file=l_nclb_location)
  } else {
    l <- readRDS(l_nclb_location)
  }
  return(l)
}
