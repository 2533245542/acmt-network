#### CRIMERISKData Settings -- Inspace ####
##download & process file functions:
download_file_crimerisk<-function(){} ### File cannot be downloaded -- Inspace partners will be provided with the raw data.

#run file processing function
process_crimerisk<-function() {
  crime_risk_raw<-read.csv('~/workspace/Inspace/raw_crimerisk.CSV')
  processed_dataframe<-crime_risk_raw %>%
    rename(total_pop_2022=POPCY) %>% #updated label to reflect that this is the total population based on 2022 census
    dplyr::select(everything(), -COUNTYNAME, -STATENAME)%>%
    melt(id='BLOCKGROUP')%>%
    rename(GEOID=BLOCKGROUP, estimate=value) %>%
    mutate(GEOID=ifelse(GEOID<100000000000, as.character(paste0('0', as.character(GEOID), "")), as.character(GEOID))) #convert to GEOID to character for joining data, need to add an extra 0 in front for some values
  
  #processed_dataframe$estimate[is.na(processed_dataframe$estimate)]<-0 #impute NA with 0 values
  
  write.csv(processed_dataframe, 'external_data/processed_crimerisk.csv', row.names = FALSE)
}

#crimerisk data presets
crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
names(crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping) = c("total_pop_2022", "CRMCYTOTC","CRMCYPERC","CRMCYMURD","CRMCYRAPE","CRMCYROBB", "CRMCYASST", "CRMCYPROC", "CRMCYBURG",  "CRMCYLARC", "CRMCYMVEH") 
external_data_presets_crimerisk <- list(vector_of_expected_downloaded_file_name=c("raw_crimerisk.csv"), ## data not downloaded, so it will be in the Inspace folder
                                        download_file=download_file_crimerisk,
                                        process_file=process_crimerisk,
                                        geoid_type="Block Group",
                                        variable_name_to_interpolate_by_sum_boolean_mapping=crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping 
)


##set up data pull specs:
external_data_name_to_info_list <- list(crimerisk=external_data_presets_crimerisk)

crimerisk_vars <-names(crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping) 
names_of_variables_to_get<-crimerisk_vars
first_var<-crimerisk_vars[1]

crimerisk_years<-c(2022)
crimerisk_selected_years<-c(2022)

crimerisk_description<-'Applied Geographic Systems publishes a block-group level dataset of ‘crime risk’. 
This pulls from the FBI’s uniform crime reports and 16000 local law enforcement jurisdictions. 
We will use area-weighted interpolation of block group-level estimates to construct buffer-specific estimates of the following measures of reported crime:'


