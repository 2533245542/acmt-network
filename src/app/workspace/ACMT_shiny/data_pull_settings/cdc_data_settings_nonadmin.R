#### CDC Places Data Settings -- Inspace ####

##set up data pull specs:
cdc_vars<-names(places_variable_name_to_interpolate_by_sum_boolean_mapping) ## set variables -- should be designated in the external_data_presets
names_of_variables_to_get<-cdc_vars
external_data_name_to_info_list <- list(places=external_data_presets_places) ## update with presets for external dataset (be sure presets are designated in the external_data-presets.R)

### get states of interest from datatset

if(file.exists('~/workspace/Inspace/dataset_geocoded.csv')==TRUE) {
dataset_geocoded<-read.csv('~/workspace/Inspace/dataset_geocoded.csv')
states_sf <- st_transform( us_states( map_date = NULL, resolution = c("low", "high"), states = NULL), 4326)
points_sf = st_as_sf(dataset_geocoded%>%filter(!is.na(lat) & !is.na(long)), coords = c("long", "lat"), crs = 4326, agr = "constant")
states <- as.data.frame( st_join(points_sf, states_sf, join = st_intersects) ) %>% dplyr::select(state_abbr, -geometry)%>%unique()%>% as.list()

}
cdc_years<-c(2017, 2018)
cdc_selected_years<-c(2017)

cdc_description<-'The Places dataset provides census tract level estimates of population behaviors taken from BRFSS responses.  
As with the ACS data, we will use area-weighted interpolation of tract-level estimates to construct buffer-specific estimates of several measures. 
PLACES was an expasion of the 500 Cities project (https://www.cdc.gov/places/about/500-cities-2016-2019/index.html) and began in 2020, 
utilizing BRFSS data from 2017 and 2018. Since then the 2021 BRFSS has also been released and utilized 2019 BRFSS data. 
Below are instructions for pulling the PLACES data using 2017/2018 and 2018/2019 BRFSS data.'