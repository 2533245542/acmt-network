## GENTRIFICATION Settings ####

gentrification_vars=c('geoid10', 't10_ldb_gen1_9000', 't10_ldb_gen1_0010')
gentrification_years<-c(2000, 2010)
gentrification_selected_years<-c(2000, 2010)

gentrification_description<-'This measure of gentrification comes from the Urban Health Collaborative (UHC) at Drexel University and conceptualize gentrification as a neighborhood change process that occurs over a period of time. The UHC measure identifies census tracts that have gentrified using a two-step process: 1) Identify census tracts that are eligible to gentrify at a specific baseline 2) Of those that were eligible, identify the tracts that show evidence of moderate or intense gentrification between baseline and a follow-up year. This results in four categories of gentrification: ineligible to gentrify (9), eligible to gentrify but did not gentrify (0), evidence of gentrification (1), intense gentrification (2).'


##functions to download and process gentrification data
download_gentrification<-function(){}

process_gentrification<-function(){
  raw_gent_data<-read.csv('~/workspace/Inspace/downloaded_gentrification.csv')
  
  processed_gentrification<- raw_gent_data %>%
    dplyr::rename(GEOID10=geoid10)%>%
    mutate(GEOID10=ifelse(GEOID10<10000000000, as.character(paste0('0', as.character(GEOID10), "")), as.character(GEOID10))) #convert to GEOID to character for joining data, need to add an extra 0 in front for some values
}


##full gentrification status ####
pull_gentrification<-function(dataset_geocoded){
  
states_sf <- st_transform( us_states( map_date = NULL, resolution = c("low", "high"), states = NULL), 4326)
points_sf = st_as_sf(dataset_geocoded%>%filter(!is.na(lat) & !is.na(long)), coords = c("long", "lat"), crs = 4326, agr = "constant")
states <- as.data.frame( st_join(points_sf, states_sf, join = st_intersects) ) %>% dplyr::select(state_abbr, -geometry)%>%unique()%>% drop_na()%>% as.list() 

#download tract shapefile
for(s in 1:length(states$state_abbr)){
  state<-states$state_abbr[s]
tracts.state<-tigris::tracts(year=2010, 
                    state=state)
if(s>1){tracts.2010<-rbind(tracts.2010, tracts.state)}
if(s==1){tracts.2010<-tracts.state}
}

#create lat-long shapefile, msa shapefile, and state shapefiles with the same projection
latlong_sf<-st_as_sf(dataset_geocoded%>%filter(!is.na(lat) & !is.na(long)), coords=c('long', 'lat'), crs=4269)
tract_2010_sf<-st_as_sf(tracts.2010, crs=st_crs(latlong_sf))
tract_2010_sf<-st_transform(tract_2010_sf, crs=st_crs(latlong_sf))

#find intersections of lat/long with tracts
intersected_tracts<-st_intersection(latlong_sf, tract_2010_sf)%>%dplyr::select(id, GEOID10)%>%st_drop_geometry()

##download & process gentrification
#download_gentrification()
process_gentrification()

raw_gent_data<-read.csv('~/workspace/Inspace/downloaded_gentrification.csv')

processed_gentrification<- raw_gent_data %>%
  dplyr::rename(GEOID10=geoid10)%>%
  mutate(GEOID10=ifelse(GEOID10<10000000000, as.character(paste0('0', as.character(GEOID10), "")), as.character(GEOID10))) #convert to GEOID to character for joining data, need to add an extra 0 in front for some values

dataset_gentrification<-merge(intersected_tracts, processed_gentrification, by='GEOID10', all.x=TRUE) %>% dplyr::select(-GEOID10)
return(dataset_gentrification)

}
