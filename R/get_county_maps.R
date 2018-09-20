get_county_maps <- function(){
  county_maps_location <- '~/Dropbox/pkg.data/nclb_segregation/raw/county_maps.rds'
  if(!file.exists(county_maps_location)){
    proj_env <- get_proj_env()
    shp_counties <- rgdal::readOGR(dsn = path.expand('~/Dropbox/pkg.data/nclb_segregation/raw/'),
                                layer='gz_2010_us_050_00_500k', 
                                stringsAsFactors = FALSE)
    shp_counties <- sp::spTransform(shp_counties, CRS(proj_env))
    shp_counties@data$CNTYFIPS <- mapply(function(x,y) paste0(x, y), x=shp_counties@data$STATE, y=shp_counties@data$COUNTY)
    county_maps <- shp_counties
    saveRDS(county_maps, file=county_maps_location)
  } else {
    county_maps <- readRDS(county_maps_location)
  }
  return(county_maps)
}