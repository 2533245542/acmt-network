#### Walkability Data Settings -- Inspace ####
external_data_name_to_info_list <- list(
  walkability=external_data_presets_walkability
)

walk_vars <-names(walkability_variable_name_to_interpolate_by_sum_boolean_mapping) 
names_of_variables_to_get<-walk_vars
first_var<-walk_vars[1]

walk_years<-c(2019)
walk_selected_years<-c(2019)

walk_description<-'The National Walkability Index is a nationwide geographic data resource that ranks block groups according to their relative walkability. 
The national dataset includes walkability scores for all block groups as well as the underlying attributes that are used to rank the block groups. 
The National Walkability Index User Guide and Methodology describes how to use the index and the methodology used to derive the index and ranked scores for its inputs.'


