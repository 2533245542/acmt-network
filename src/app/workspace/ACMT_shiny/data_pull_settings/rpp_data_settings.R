## RPP Settings ####

rpp_vars=c('GEOID_pp', 'state_geoid', 'msa_geoid', 'GeoName', 'year', 'rpp_goods', 'rpp_services_housing', 'rpp_services_other', 'rpp_services_utilities', 'rpp_all_items')
rpp_years<-c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
rpp_selected_years<-c(2015)

rpp_description<-'Regional price parities (RPPs) measure the differences in price levels across states and metropolitan areas for a given year and are expressed as a percentage of the overall national price level. States with the highest RPPs in 2020 were Hawaii (112.0), New Jersey (111.2), and California (110.4); the RPP in the District of Columbia was 111.5. States with the lowest RPPs were Mississippi (87.8), West Virginia (88.0), and Arkansas (89.2). The release also includes new estimates of 2020 regional price parities for the metropolitan areas and revised data for the states and metropolitan areas for 2008 to 2019.'

pull_rpp_ids<-function(dataset_geocode){
#download cbsa and state shapefile
cbsa<-core_based_statistical_areas(cb=TRUE,year=2019) # download CBSA shapefiles 

cbsa.data<-as.data.frame(cbsa)
state<-states(year=2019)

#create lat-long shapefile, msa shapefile, and state shapefiles with the same projection
latlong_sf<-st_as_sf(dataset_geocoded%>%filter(!is.na(lat) & !is.na(long)), coords=c('long', 'lat'), crs=4269)
cbsa_sf<-st_transform(st_as_sf(cbsa, crs=4269), crs=st_crs(latlong_sf))
state_sf<-st_transform(st_as_sf(state, crs=4269), crs=st_crs(latlong_sf))

#find intersections of lat/long with state and msa (filter msa file to just metro areas [i.e., remove the micro areas])
intersected_msa<-st_intersection(latlong_sf, cbsa_sf)%>%filter(LSAD=='M1')%>%dplyr::select(id, msa_geoid=GEOID)%>% st_drop_geometry()
intersected_state<-st_intersection(latlong_sf, state_sf)%>%dplyr::select(id, state_geoid=GEOID)%>%st_drop_geometry()

msa_state_dataset <- merge(dataset_geocoded %>%dplyr::select(id), intersected_msa, by='id', all=TRUE)
msa_state_dataset<-merge(msa_state_dataset, intersected_state, by='id', all=TRUE)
msa_state_dataset<-msa_state_dataset%>%mutate(GEOID_pp=ifelse(is.na(msa_geoid), state_geoid, msa_geoid))

write.csv(msa_state_dataset, '~/workspace/Inspace/msa_state_dataset.csv')

}

