source("GeocoderACMT.R")


convert_list_of_dataframe_to_sf <- function (list_of_dataframe) {
  list_of_converted_dataframe <- list()
  for (name in names(list_of_dataframe)) {
    list_of_converted_dataframe[[name]] <- st_as_sf(list_of_dataframe[[name]], coords = c("longitude", "latitude"), crs = 4326)
  }
  return(list_of_converted_dataframe)
}

get_one_buffer_one_dataset_aggregated_info <- function (created_buffer=NULL, coordinate_dataframe_sf=NULL, aggregation_type='count', variables_of_interest=NULL) {
  intersected_dataframe_sf <- st_intersection(coordinate_dataframe_sf, created_buffer)
  intersected_dataframe <- st_drop_geometry(intersected_dataframe_sf)
  aggregated_dataframe <- intersected_dataframe %>%
    group_by(variable) %>%
    summarise(aggregated_estimate=sum(estimate))
  return(aggregated_dataframe)
}

get_aggregated_point_measures <- function (latitude=-122.333, longitude=47.663, radius=2000, external_data_name_to_info_list=NULL, custom_buffer=NULL) {
  stopifnot("Please provide external_data_name_to_info_list"= !is.null(external_data_name_to_info_list))
  # create radial buffer
  if (is.null(custom_buffer)) {
    created_buffer <- get_point_buffer_for_lat_long(lat=latitude, long=longitude, radius_meters=radius)
  } else {
    created_buffer <- custom_buffer
  }
  # load a list of datasets
  external_data_list <- load_external_data(external_data_name_to_info_list)
  # convert list of datasets to sf format
  converted_external_data_list <- convert_list_of_dataframe_to_sf(list_of_dataframe=external_data_list)
  # aggregate info for each dataset
  variable_to_aggregated_dataframe_list <- list()
  for (external_data_name in names(external_data_name_to_info_list)) {
    aggregated_dataset <- get_one_buffer_one_dataset_aggregated_info(created_buffer = created_buffer, coordinate_dataframe_sf = converted_external_data_list[[external_data_name]])
    variable_to_aggregated_dataframe_list[[external_data_name]] <- aggregated_dataset
  }
  all_aggregated_dataframe <- do.call(rbind, variable_to_aggregated_dataframe_list)
  result_list <- list(created_buffer=created_buffer,
                      external_data_list=external_data_list,
                      converted_external_data_list=converted_external_data_list,
                      variable_to_aggregated_dataframe_list=variable_to_aggregated_dataframe_list,
                      aggregated_result=all_aggregated_dataframe)
  return(result_list)
}