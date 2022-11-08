# the working directory should be
load_external_data <- function (external_data_name_to_info_list=NULL) {
  if (!dir.exists("external_data")) {
    dir.create("external_data")
  }

  vector_of_external_data_names <- names(external_data_name_to_info_list)
  list_of_loaded_data_dataframe <- c()

  vector_of_existing_file_names <- list.files("external_data")

  for (external_data_name in vector_of_external_data_names) {
    vector_of_expected_downloaded_file_name <- external_data_name_to_info_list[[external_data_name]]$vector_of_expected_downloaded_file_name

    expected_processed_file_name <- external_data_name_to_info_list[[external_data_name]]$expected_processed_file_name
    if(is.null(expected_processed_file_name)) {  # when expected_processed_file_name not specified in the info list
      expected_processed_file_name <- paste0("processed_", external_data_name, ".csv")
    }

    file_downloading_completed <- is_empty(setdiff(vector_of_expected_downloaded_file_name, vector_of_existing_file_names))  # could have multiple downloaded files
    file_processing_completed <- expected_processed_file_name %in% vector_of_existing_file_names  # could only have one processed file

    download_file <- external_data_name_to_info_list[[external_data_name]]$download_file
    process_file <- external_data_name_to_info_list[[external_data_name]]$process_file

    if (file_processing_completed) {

    } else {
      if (file_downloading_completed) {
        process_file()
      } else {
        download_file()
        process_file()
      }
    }

    list_of_loaded_data_dataframe[[external_data_name]] <- read_csv(paste0("external_data/", expected_processed_file_name), col_types=cols('GEOID'=col_character()))
  }
  return(list_of_loaded_data_dataframe)
}

