
## Introduction Links to measures ####
measures.list<-c('<a href = "https://www.census.gov/programs-surveys/acs/about.html" target = "new" >The American Community Survey</a>',
                 '<a href = "https://www.epa.gov/smartgrowth/smart-location-mapping#walkability" target = "new" >Walkability Index</a>',
                 '<a href = "https://www.cdc.gov/places/index.html" target = "new" >CDC PLACES data</a>',
                 '<a href = "https://www.usgs.gov/centers/eros/science/national-land-cover-database" target = "new" >National Land Cover Database</a>',
                 '<a href = "https://www.cdc.gov/obesity/downloads/census-tract-level-state-maps-mrfei_TAG508.pdf" target = "new" >Modified Retail Food Environment Index (mRFEI)</a>',
                 '<a href = "https://www.tpl.org/parkserve" target = "new" >Trust for Public Lands’ ParkServe </a>',
                 '<a href = "https://appliedgeographic.com/crimerisk/" target = "new" >Applied Geographic Solutions CrimeRisk Data</a>',
                 '<a href = "https://journals.sagepub.com/doi/10.1177/0033354920968799" target = "new" >Sidewalk Score</a>',
                 '<a href = "https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area" target = "new" >Regional Price Parity</a>',
                 '<a href = "https://drexel.edu/uhc/resources/briefs/Measure-of-Gentrification-for-Use-in-Longitudinal-Public-Health-Studies-in-the-US/" target = "new" >Gentrification Measure</a>')


### example datasets ###

example_geocoded<-data.frame(id=c('1', '2', '3'), lat=c(47.57624, 47.52617, 48.67608), long=c(-122.294, -122.27012, -122.37496))
example_address<-data.frame(id=c('1', '2', '3'), address=c('8601 Rainer Ave. S, Seattle, WA 98118', 
                                                           '1418 NW 65th Street., Seattle, WA 98117', 
                                                           '2600 SW Thistle Street, Seattle, WA 98125'))

### add columns and loop data through geocoder ####
geocode_loop<-function(dataset){
dataset_geocode<-dataset %>%
  mutate(lat=NA, 
         long=NA, 
         rating=NA)
### Geocoding loop ####
for (i in 1:nrow(dataset_geocode)) {
  print(i)
  if(!is.na(dataset_geocode$lat[i])) next #skip already geocoded
  if(is.na(dataset_geocode$address[i])) next #skip NA address values
  address<-dataset_geocode$address
  lat_long<-tryCatch({geocode(address[i])}, 
                     error=function(x){
                       lat_long<-list(latitude=NA, longitude=NA, rating=NA)
                     })
  dataset_geocode$lat[i]<-lat_long$latitude # add latitude to dataset_geocode
  dataset_geocode$long[i] <-lat_long$longitude # add longitude to dataset_geocode
  dataset_geocode$rating[i]<-lat_long$rating # add rating to the dataset_geocode
}
dataset_geocode<-dataset_geocode %>% arrange(-rating)
dataset_geocode
}

#print table of overall geocode ratings ####
ratings_table<-function(dataset){
ratings.table<-dataset %>%
  mutate(rating_group=ifelse(rating==0, '0', ifelse(rating>0 & rating<10, '0 to 9', ifelse(rating>9 & rating < 21, '10-20', ifelse(rating>20, '20 or higher', NA)))))%>%group_by(rating_group)%>%summarise(n=n())%>%mutate(percent=round(n/sum(n), 2)) %>%
  dplyr::select(rating_group, n, percent)

return(ratings.table)

}

### Map Geocodes ####
check_geocode <- function(lat, long, address, rate, id, z=16, side_len=0.007){
  bbox <- c(left = long - side_len, bottom = lat - side_len, right = long + side_len, top = lat + side_len)
  map <- leaflet() %>% addTiles()%>% addCircleMarkers(lng=long, lat=lat, label=paste(paste('ID = ', id, sep=''), address, paste("Rating =", rate, sep = ""), sep='\n'))
  return(map)
}

#check_geocode_for_address <- function(address, id, z=17, side_len=0.007){
#  location <- geocode(address = address)
check_geocode_for_address<-function(lat, long, id, address=address, rate=rating, z=17, side_len=0.007){
  map <- check_geocode(lat = lat, long = long, address = address, rate=rate, id=id, z=z, side_len=side_len)
  return(map)
}


## create data pull measures folder if it doesn't exist: 
if(dir.exists('~/workspace/Inspace/data_pull_measures')==FALSE){
  dir.create('~/workspace/Inspace/data_pull_measures')
}

## create data pull summaries folder if it doesn't exist: 
if(dir.exists('~/workspace/Inspace/data_pull_summaries')==FALSE){
  dir.create('~/workspace/Inspace/data_pull_summaries')
}

### save data frame to Inspace Folder ####

outputDir <- "~/workspace/Inspace"

saveData <- function(data, fileName) {
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

### Load dataset from Inspace folder ####
loadData <- function(file.name) {
  # Read all the files into a list
  data <- read.csv(file=paste0(outputDir, '/', file.name))
  # Concatenate all data together into one data.frame
    data
}

reloadData<-function(file.name){
  data<-read.csv(file=paste0(outputDir, '/', file.name))
  data
}

### Create dataframe to pull data ####
create_dataset<-function(variable_list=variable_list){
  var.cols<-data.frame(matrix(ncol=length(variable_list)))#make a columns for each variable in the list
  colnames(var.cols)<-variable_list #name the columns
  dataset<-var.cols%>%mutate(id=NA, year=NA, radius=NA)%>%filter(!is.na(id))#add the columns to the dataset
  return(dataset)
}

### Pull County GEOID for each lat/long ####

pull_county_geoid<-function(dataset, year=2019){
 
  latlong_sf<-st_as_sf(dataset_geocoded%>%dplyr::select(id, lat, long)
                       %>% filter(!is.na(lat) & !is.na(long)), 
                       coords=c('long', 'lat'), crs=4269)
  counties_sf<-st_transform(counties, crs=4269)
  intersected_county<-st_intersection(latlong_sf, counties_sf)
  
  #msa_dataset <- merge(dataset_acs %>%dplyr::select(id), intersected_msa, by='id', all=TRUE)%>% dplyr::select(id, #msa_geoid=GEOID)
  county_dataset<-merge(dataset_geocoded%>%dplyr::select(id), intersected_county, by='id', all=TRUE)%>%dplyr::select(id, county_geoid=GEOID, -geometry)%>%unique()
  
  write.csv(county_dataset, '~/workspace/Inspace/data_pull_measures/dataset_county.csv')
}






### Summary Table of environmental measures ####
table_missingness<-function(dataset) {
  summary_table_na<-dataset %>% group_by(radius, year) %>% summarise(count_na=sum(is.na(.)), 
                                                                  count_total=n())%>%arrange(year)
  return(summary_table_na)
}
table_summary<-function(dataset) {
  
  data_summary<-cbind(colnames(dataset)%>% as.data.frame() %>% rename(., variable_name=.) %>% dplyr::filter(!(variable_name %in% c('id', 'year', 'radius'))),
                      
        dataset %>% dplyr::select(-id, -year, -radius) %>% summarise_all(list(min=min), na.rm=TRUE) %>%t()%>%as.data.frame() %>% round(., digits=2) %>% rename(min=V1), 
        dataset %>% dplyr::select(-id, -year, -radius) %>% summarise_all(list(max=max), na.rm=TRUE) %>%t()%>%as.data.frame() %>% round(., digits=2)%>% rename(max=V1), 
        dataset %>% dplyr::select(-id, -year, -radius) %>% summarise_all(list(median=median), na.rm=TRUE) %>%t()%>%as.data.frame() %>% round(., digits=2)%>% rename(median=V1), 
        dataset %>% dplyr::select(-id, -year, -radius) %>% summarise_all(~sum(length(which(is.na(.))))) %>%t()%>%as.data.frame() %>% rename(NA_count=V1))
  row.names(data_summary)<-NULL
  return(data_summary)
        
}

measure.directory<-'~/workspace/Inspace/data_pull_measures/'
progress.summary<-function(filename){
  file.path<-paste0(measure.directory, filename)
  if(file.exists(file.path) == FALSE){
  return(0)}
  if(file.exists(file.path) == TRUE){
    if('X' %in% colnames(read.csv(file.path))){
      nrow(read.csv(file.path) %>%dplyr::select(-X) %>%  distinct())
    }
    else{
    nrow(read.csv(file.path) %>% distinct())}
  }
}

