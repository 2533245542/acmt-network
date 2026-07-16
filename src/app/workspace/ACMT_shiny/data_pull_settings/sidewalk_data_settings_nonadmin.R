#### sidewalk score Data Settings -- Inspace ####
download_file_sidewalk<-function(){}

## section: Sidewalk View
process_sidewalk<-function() {
  raw_sidewalk<-read.csv('Inspace/downloaded_sidewalk.csv')
  processed_dataframe<-raw_sidewalk %>%
    dplyr::select(censustract, total_num, total_crosswalk, total_sidewalk) %>%
    melt(id='censustract')%>%
    rename(GEOID=censustract, estimate=value) %>%
    mutate(GEOID=as.character(GEOID)) %>%
    mutate(GEOID=ifelse(nchar(GEOID)<11, paste0('0', GEOID), GEOID)) #convert to GEOID to character for joining data, need to add an extra 0 in front for some values
  processed_dataframe$estimate[is.na(processed_dataframe$estimate)]<-0 #impute NA with 0 values
  
  write_csv(processed_dataframe, 'Inspace/external_data/processed_sidewalk.csv')
}


#sidewalk presents
sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping = c(TRUE, TRUE, TRUE)
names(sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping) = c('total_num', 'total_crosswalk', 'total_sidewalk')
external_data_presets_sidewalk <- list(vector_of_expected_downloaded_file_name=c("downloaded_sidewalk.csv"),
                                       download_file=download_file_sidewalk,
                                       process_file=process_sidewalk,
                                       geoid_type="Census Tract",
                                       variable_name_to_interpolate_by_sum_boolean_mapping=sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping 
)


external_data_name_to_info_list <- list(
  sidewalk=external_data_presets_sidewalk
)

sidewalk_vars <-names(sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping) 
names_of_variables_to_get<-sidewalk_vars
first_var<-sidewalk_vars[1]

sidewalk_years<-c(2017)
sidewalk_selected_years<-c(2017)

sidewalk_description<-'T Quynh Nguyen at University of Maryland has developed a national sidewalk presence dataset by applying machine learning to Google Street View images to produce tract-level estimates of sidewalk presence/panoramic image by tract. Measures included percent of images for a given census tract that has crosswalks, the percent of images for a given census tract with a sidewalk We will use area-weighted interpolation to construct buffer specific measures of sidewalk presence. .'


sidewalk_zscores<-function(dataset_sidewalk){
dataset_sidewalk<-dataset_sidewalk%>%
  mutate(prop_sidewalk = total_sidewalk/total_num, 
         prop_crosswalk=total_crosswalk/total_num)

#calculate mean prop_sidewalk and mean prop_crosswalk
mean.sidewalk=mean(dataset_sidewalk$prop_sidewalk, na.rm = TRUE)
mean.crosswalk=mean(dataset_sidewalk$prop_crosswalk, na.rm=TRUE)
sd.sidewalk=sd(dataset_sidewalk$prop_sidewalk, na.rm=TRUE)
sd.crosswalk=sd(dataset_sidewalk$prop_crosswalk, na.rm=TRUE)

#calculate sidewalk and crosswalk z-scores
dataset_sidewalk<- dataset_sidewalk%>%
  mutate(sidewalk_z=(prop_sidewalk-mean.sidewalk)/sd.sidewalk, 
         crosswalk_z=(prop_crosswalk-mean.crosswalk)/sd.crosswalk)

}