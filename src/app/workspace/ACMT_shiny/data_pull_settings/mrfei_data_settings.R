##mRFEI Data Settings 
#Designate interpolation strategy for variables
external_data_name_to_info_list <- list(
  mrfei=external_data_presets_mrfei
)

mrfei_vars <-names(mrfei_variable_name_to_interpolate_by_sum_boolean_mapping) 
names_of_variables_to_get<-mrfei_vars
first_var<-mrfei_vars[1]

mrfei_years<-c(2011)
mrfei_selected_years<-c(2011)

mrfei_description<-'The CDC’s modified retail food environment index provides census tract level estimates the ratio of healthy food outlets 
to total food outlets. 
We will use area-weighted interpolation of tract-level estimates to construct buffer-specific estimates of the following measures.'
