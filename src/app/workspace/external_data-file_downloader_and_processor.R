# Instructions for building costomized download_file(()
# download_file() has no input and output.
# ACMT first checks if `vector_of_expected_downloaded_file_name` are downloaded (exist in external_data); if they are not downloaded, ACMT runs the respective download_file() to download the files.

# Instructions for building costomized process_file()
# process_file()
# When creating `external_data_name_to_info_list`, a name needs to be given for this dataset (e.g. mrfei in the example). ACMT retreives the names of the external dataset by running names(external_data_name_to_info_list).
# This name defines the name of the processed file. For example, when the dataset name is mrfei, the process_file function should create a .csv file called `processed_mrfei.csv`. Only processed file should be created for each external dataset.
# ACMT infers the name of the processed dataset and checks if it exists; if it does not exist, ACMT runs process_file().
# It is recommended to use tidyverse's write_csv to generate the processed csv file because read_csv will be used in external_data-file_loader.R
# The processed csv file should follow the below format. The first column is the GEOID, the second column is the variable name, the third column is the variable's value. Here we have two variables, namely B01001_001 and B01001_002, for the same location and they have different values.

#GEOID       variable   estimate
#53033000100 B01001_001     6282
#53033000100 B01001_002     2914

# Instructions for advanced users
# You can just drag the processed file from elsewhere into the external_data folder, and set download_file and process_file to be empty functions. ACMT runs properly in this way as well because ACMT can run as long as the processed file is availble.

library(readxl)

# section: mRFEI data https://www.cdc.gov/obesity/resources/reports.html
download_file_mrefi <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  download.file(url = "https://www.cdc.gov/obesity/downloads/2_16_mrfei_data_table.xls", destfile = "external_data/downloaded_mrfei.xls")
}

process_file_mrefi <- function () {  # process the file and name it processed_mrfei.csv (mrfei can change depending on how you name your dataset in external_data_name_to_info_list, but "processed" and ".csv" are fixed characters)
  raw_mrfei_dataframe <- read_excel("external_data/downloaded_mrfei.xls")

  ## select the GEOID and estimate columns (rename with proper names), and insert the varaible column
  processed_dataframe <- raw_mrfei_dataframe %>%
    dplyr::select(fips, mrfei) %>%
    rename(GEOID=fips, estimate=mrfei) %>%
    add_column(variable="mRFEI", .after = "GEOID")

  processed_dataframe$estimate[is.na(processed_dataframe$estimate)] <- 0  # impute NA with 0

  write_csv(processed_dataframe, "external_data/processed_mrfei.csv")
}