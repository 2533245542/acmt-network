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

# Note: after processing, the GEOID should all have 11 digits (at the Census Tract level) https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html

#GEOID       variable   estimate
#53033000100 B01001_001     6282
#53033000100 B01001_002     2914
#09011650100 B01001_001     2910
#09011650100 B01001_002     9913

# Instructions for advanced users
# You can just drag the processed file from elsewhere into the external_data folder, and set download_file and process_file to be empty functions. ACMT runs properly in this way as well because ACMT can run as long as the processed file is availble.


# section: mRFEI data https://www.cdc.gov/obesity/resources/reports.html
download_file_mrefi <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  download.file(url = "https://www.cdc.gov/obesity/downloads/2_16_mrfei_data_table.xls", destfile = "external_data/downloaded_mrfei.xls")
}

process_file_mrefi <- function () {  # process the file and name it processed_mrfei.csv (mrfei can change depending on how you name your dataset in external_data_name_to_info_list, but "processed" and ".csv" are fixed characters)
  library(readxl)
  raw_mrfei_dataframe <- read_excel("external_data/downloaded_mrfei.xls")

  ## select the GEOID and estimate columns (rename with proper names), and insert the varaible column
  processed_dataframe <- raw_mrfei_dataframe %>%
    dplyr::select(fips, mrfei) %>%
    rename(GEOID=fips, estimate=mrfei) %>%
    add_column(variable="mRFEI", .after = "GEOID")

  processed_dataframe$estimate[is.na(processed_dataframe$estimate)] <- 0  # impute NA with 0

  write_csv(processed_dataframe, "external_data/processed_mrfei.csv")
}

# section: National Walkability Index https://edg.epa.gov/metadata/catalog/search/resource/details.page?uuid=%7B251AFDD9-23A7-4068-9B27-A3048A7E6012%7D
download_file_walkability <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  default_timeout_duration <- getOption('timeout')  # this download would take a long time
  options(timeout=99999999)
  download.file(url = "ftp://newftp.epa.gov/EPADataCommons/OP/WalkabilityIndex.zip", destfile = "external_data/downloaded_walkability.zip")
  options(timeout=default_timeout_duration)
}

process_file_walkability <- function () {  # process the file and name it processed_mrfei.csv (mrfei can change depending on how you name your dataset in external_data_name_to_info_list, but "processed" and ".csv" are fixed characters)
  # variables used
  #{
  #COUNTHU10: Housing units, 2010
  #TOTPOP10: Population, 2010
  #HH: Households (occupied housing units), 2010
  #WORKERS: Number of workers in block group (home location), 2010
  #AC_TOT: Total geometric area of block group in acres
  #AC_WATER: Total water area of block group in acres
  #AC_LAND: Total land area of block group in acres
  #AC_UNPR: Total land area that is not protected from development (i.e., not a park or conservation area) of block group in acres
  ## D2A_EPHHM: Employment and household entropy
  ## D2B_E8MIXA: 8-tier employment entropy (denominator set to the static 8 employment types in the CBG)
  ## D3b: Street intersection density (weighted, auto-oriented intersections eliminated)
  ## D4a: Distance from population weighted centroid to nearest transit stop (meters)
  ## D2A_Ranked: Ranked score of block group's D2a_EPHMM value relative to other block groups
  ## D2B_Ranked: Ranked score of block group's D2b_E8mixA value relative to other block groups
  ## D4A_Ranked: Ranked score of block group's D4a value relative to other block groups
  ## D3B_Ranked: Ranked score of block group's D3b value relative to other block groups
  #NatWalkInd: The score represents the relative walkability of a block group compared to other block groups
  ## Shape_Length: Length of feature in internal units.
  ## Shape_Area: Area of feature in internal units squared.
  #}

  # get the shape files
  unzip("external_data/downloaded_walkability.zip", exdir="external_data/unzipped_walkability")
  unzip("external_data/unzipped_walkability/Natl_WI.gdb.zip", exdir="external_data/unzipped_walkability")

  national_walkability_index_sf <- sf::st_read(dsn="external_data/unzipped_walkability/Natl_WI.gdb", layer="NationalWalkabilityIndex")

  # select only the variables of intersts and makes sense in ACMT (can interpolate across census tracts)
  variables_to_select <- c("GEOID10", "COUNTHU10", "TOTPOP10", "HH", "WORKERS", "AC_TOT", "AC_WATER", "AC_LAND", "AC_UNPR", "NatWalkInd")  # GEOID plus the 9 variables
  selected_national_walkability_index_sf <- national_walkability_index_sf[variables_to_select]

  # convert to a normal dataframe
  selected_national_walkability_index_dataframe <- selected_national_walkability_index_sf %>%
    as_tibble() %>%
    dplyr::select(-Shape)

  # reformat table to match ACMT format
  acmt_formatted_walkability_index_dataframe <- selected_national_walkability_index_dataframe %>%
    gather("variable", "estimate", -GEOID10) %>%
    rename(GEOID=GEOID10)

  # contains NA? We checked and this file has no NA

  # write to processed_walkability.csv
  write_csv(acmt_formatted_walkability_index_dataframe, "external_data/processed_walkability.csv")
}
