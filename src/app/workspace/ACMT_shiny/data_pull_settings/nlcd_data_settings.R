#### NLCD Data Settings -- ACMT_shiny ####
library(magrittr)
source('~/workspace/ACMT_shiny/data_pull_settings/NLCD_UTILITY_FUNCTIONS.R')

## set NLCD settings###
##Create Variable Legend ####
var.code<-c(11, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95)
var.name<-c('open_water', 'develop_openspace', 'develop_medintense', 'develop_lowintense', 'develop_highintense',
            'barren_land', 'decid_forest', 'evergreen_forest',  'mixed_forest',   'dwarf_scrub',  
            'shrub_scrub',  'herbacious',   'sedge_herb',  'lichen','moss', 'pasture','cultivate_crops','wood_wetlands','herb_wetlands')
nlcd_legend<-data.frame(x=var.code, legend=var.name)
#set vars and years
nlcd_vars<-nlcd_legend$legend
nlcd_years<-c(2004, 2006, 2008, 2011, 2013, 2016, 2019)
nlcd_selected_years<-c(2016)

nlcd_description<-'The National Land Cover Database uses satelitte imagery to create pixel-by-pixes maps of land cover features. Areas of 30-meters are categorized into land cover classes including forest, low-, medium-, and high-intensity developed, and more. For more information about the NLCD, visit this link: https://www.usgs.gov/news/technical-announcement/new-land-cover-maps-capture-nearly-two-decades-change-across-us'


##lat/long for testing####
#lat<-dataset_geocoded$lat[5]
#long<-dataset_geocoded$long[5]
#radius<-1000
#year<-2019

post_process_nlcd<-function(variable_list, prop.nlcd){
  prop.nlcd<-data.frame(prop.nlcd)
  if(nrow(prop.nlcd)==0){
    prop.nlcd<-data.frame(x=NA, Freq=NA)
  }  
  environmental_measures<-merge(nlcd_legend, prop.nlcd, by=c('x'), all.x=TRUE)
  environmental_measures[is.na(environmental_measures)]<-0
  return(environmental_measures)
}

pull_nlcd_data<-function(year, label, dataset='landcover', landmass = "L48", force.redo=FALSE){
  
  dataset <- match.arg(dataset)
  dataset <- switch(dataset,
                    landcover = "Land_Cover",
                    impervious = "Impervious",
                    canopy = "Tree_Canopy"
  )
  
  # coverage <- paste0("NLCD_", year, "_", dataset, "_", landmass)
  # source <- "https://www.mrlc.gov/geoserver/wcs"
  
  extraction.dir = paste0(
    tempdir(),
    "/FedData/"
  )
    extraction.dir <-
      normalizePath(paste0(extraction.dir, "/."), mustWork = FALSE)
  
  dir.create(extraction.dir, showWarnings = FALSE, recursive = TRUE)
  
  outfile <-
    paste0(extraction.dir, "/", label, "_NLCD_", dataset, "_", year, ".tif")
  
  if (file.exists(outfile) & !force.redo) {
    return(raster::raster(outfile))
  }
  
  source <- "https://storage.googleapis.com/feddata-r/nlcd/"
  file <- paste0(year, "_", dataset, "_", landmass, ".tif")
  
  path <- paste0(source, file)
  
  if (path %>%
      httr::HEAD() %>%
      httr::status_code() %>%
      identical(200L) %>%
      magrittr::not()) {
    stop(
      "NLCD data are not available for dataset '", dataset, "', year '", year,
      "', and landmass '", landmass,
      "'. Please see available datasets at https://www.mrlc.gov/data."
    )
  }
  
  # template %<>%
  #   template_to_sf()
  
  out <-
    paste0("/vsicurl/", path) %>%
    #terra::rast() %>%
    raster()
  return(out)
}

pull_nlcd_measures<-function(longitude, latitude, radius, variable_list,  nlcd_data){
  #create buffer shapefile
  buffer1<-st_sf(get_point_buffer_for_lat_long(longitude, latitude, radius_meters=radius))
  #nlcd_data<-pull_nlcd_data(year=year, label='nlcd landcover')
  ##extract NLCD data for buffer
  nlcd_extract<-raster::extract(nlcd_data, buffer1)
  #calculate proportion of each type of land cover, convert prop.table to dataframe
  prop.nlcd<-lapply(nlcd_extract, FUN=function(x) {prop.table(table(x))})
  
  environmental_measures<-post_process_nlcd(variable_list=nlcd_vars, prop.nlcd)
}






