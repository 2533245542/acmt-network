#' interpolatable data
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
# section: mRFEI data https://www.cdc.gov/obesity/downloads/census-tract-level-state-maps-mrfei_TAG508.pdf
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

# section: USDA Food Access https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data/
download_file_food_access <- function () {
  download.file(url = "https://www.ers.usda.gov/webdocs/DataFiles/80591/DataDownload2015.xlsx?v=7588.8", destfile = "external_data/downloaded_food_access.xlsx")
}
process_file_food_access <- function () {
  library(readxl)
  library(tidyverse)
  raw_food_access_dataframe <- read_excel(path="external_data/downloaded_food_access.xlsx", sheet = "Food Access Research Atlas")
  state_county_pruned_food_access_dataframe <- dplyr::select(as_tibble(raw_food_access_dataframe), -c(State, County))

  acmt_formatted_food_access_dataframe <- state_county_pruned_food_access_dataframe %>%
    gather(key="variable", value="estimate", -CensusTract) %>%
    rename(GEOID=CensusTract)

  write_csv(acmt_formatted_food_access_dataframe, "external_data/processed_food_access.csv")
}

# section: NO2 http://spatialmodel.com/concentrations/Bechle_LUR.html#annual
download_file_no2 <- function () {
  download.file(url = "https://github.com/spatialmodel/concentrations/releases/download/Bechle_blockgroups/BechleLUR_BlockGroup_Annual.txt", destfile = "external_data/downloaded_no2.csv")
}
process_file_no2 <- function () {
  library(tidyverse)
  raw_no2 <- read_csv("external_data/downloaded_no2.csv")
  processed_no2 <- raw_no2 %>%
    dplyr::select(STFID, Y2010) %>%
    add_column(variable="NO2", .before = "Y2010") %>%
    rename(GEOID=STFID, estimate=Y2010)

  write_csv(processed_no2, "external_data/processed_no2.csv")
}

# section: O3 https://www.epa.gov/hesc/rsig-related-downloadable-data-files
download_file_o3 <- function () {
  download.file(url = "https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/outputs/2017_ozone_daily_8hour_maximum.txt.gz", destfile = "external_data/downloaded_o3.txt.gz")
}
process_file_o3 <- function () {
  # one record per one day, one GEOID, long and lat
  library(tidyverse)

  raw_o3 <- read_csv("external_data/2017_ozone_daily_8hour_maximum.txt")
  colnames(raw_o3) <- c("Date", "FIPS", "Longitude", "Latitude", "O3", "O3_stderr")
  raw_o3$O3 <- as.numeric(raw_o3$O3)  # original name is not friendly for programming
  raw_o3$O3_stderr <- as.numeric(raw_o3$O3_stderr)
  aggregated_raw_o3 <- raw_o3 %>%
    dplyr::select(-Date, -Longitude, -Latitude) %>%
    group_by(FIPS) %>%
    summarise(O3=mean(O3), O3_stderr=mean(O3_stderr)) %>%
    rename(GEOID=FIPS)

  processed_o3 <- aggregated_raw_o3 %>%
    gather(key="variable", value="estimate", -GEOID)

  write_csv(processed_o3, "external_data/processed_o3.csv")
}

# section: PM2.5 https://www.epa.gov/hesc/rsig-related-downloadable-data-files
download_file_pm25 <- function () {
  download.file(url = "https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/outputs/2017_pm25_daily_average.txt.gz", destfile = "external_data/downloaded_pm25.txt.gz")
}
process_file_pm25 <- function () {
  library(tidyverse)
  raw_pm25 <- read_csv("external_data/2017_pm25_daily_average.txt", col_types = "Dcdddd")
  colnames(raw_pm25) <- c("Date", "GEOID", "Longitude", "Latitude", "PM25", "PM25_stderr")
  aggregated_pm25 <- raw_pm25 %>%
    dplyr::select(-Date, -Longitude, -Latitude) %>%
    group_by(GEOID) %>%
    summarise(PM25=mean(PM25), PM25_stderr=mean(PM25_stderr))
  processed_pm25 <- aggregated_pm25 %>%
    gather(key="variable", value="estimate", -GEOID)

  write_csv(processed_pm25, "external_data/processed_pm25.csv")
}

#' point estimate data

# section: 911 calls https://data.seattle.gov/Public-Safety/Seattle-Real-Time-Fire-911-Calls/kzjm-xkqj
download_file_call911 <- function () {
  # download .csv file Seattle Real Time Fire 911 Calls
}
process_file_call911 <- function () {
  library(tidyverse)
  raw_call911 <- read_csv("external_data/downloaded_call911.csv", col_types = "cccddcc")
  colnames(raw_call911) <- c("Address", "Type", "Datetime", "Latitude", "Longitude", "ReportLocation", "IncidentNumber")
  call911_selected <- raw_call911 %>%  # select 2015 start to 2020 end data
    dplyr::select(Type, Datetime, Latitude, Longitude)

  call911_selected$Datetime <-
    as.Date(substr(call911_selected$Datetime, start = 1, stop = 10), format = "%m/%d/%Y")

  call911_selected_2015_to_2020 <- call911_selected %>%
    filter(Datetime > "2015-01-01" & Datetime < "2020-12-31")

  processed_call911 <- call911_selected_2015_to_2020 %>%
    dplyr::select(-Datetime) %>%
    rename(variable=Type, latitude=Latitude, longitude=Longitude) %>%
    add_column(estimate=1, .before = "latitude") %>%
    mutate(variable=str_c("is_call911 ", variable)) %>%
    drop_na(latitude, longitude)  # dropped 563 rows out of 500362 rows

  write_csv(processed_call911, "external_data/processed_call911.csv")
}

# section: crime seattle
download_file_crime_seattle <- function () {
}
process_file_crime_seattle <- function () {
  library(tidyverse)
  raw_crime_seattle <- read_csv("external_data/downloaded_crime_seattle.csv")

  crime_seattle_selected <- raw_crime_seattle %>%
    dplyr::select(`Report DateTime`, Offense, Latitude, Longitude)

  crime_seattle_selected$`Report DateTime` <-
    as.Date(substr(crime_seattle_selected$`Report DateTime`, start = 1, stop = 10), format = "%m/%d/%Y")

  crime_seattle_selected_2020 <- crime_seattle_selected %>%
    filter(`Report DateTime` >= "2020-01-01" & `Report DateTime` <= "2020-12-31")

  processed_crime_seattle <- crime_seattle_selected_2020 %>%
    dplyr::select(-`Report DateTime`) %>%
    rename(variable=Offense, latitude=Latitude, longitude=Longitude) %>%
    add_column(estimate=1, .before = "latitude") %>%
    mutate(variable=str_c("is_crime_seattle ", variable)) %>%
    filter(latitude!=0 & longitude!=0)

  write_csv(processed_crime_seattle, "external_data/processed_crime_seattle.csv")
}

# section: crime boston https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system
download_file_crime_boston <- function () {
}
process_file_crime_boston <- function () {
  library(tidyverse)
  raw_crime_boston <- read_csv("external_data/downloaded_crime_boston.csv")

  crime_boston_selected <- raw_crime_boston %>%
    dplyr::select(OCCURRED_ON_DATE, OFFENSE_DESCRIPTION, Lat, Long)

  crime_boston_selected$OCCURRED_ON_DATE <- as.Date(substr(crime_boston_selected$OCCURRED_ON_DATE, start = 1, stop = 10), format = "%Y-%m-%d")

  processed_crime_boston <- crime_boston_selected %>%
    dplyr::select(-OCCURRED_ON_DATE) %>%
    rename(variable=OFFENSE_DESCRIPTION, latitude=Lat, longitude=Long) %>%
    add_column(estimate=1, .before = "latitude") %>%
    mutate(variable=str_c("is_crime_boston ", variable)) %>%
    filter(latitude!=0 & longitude!=0)

  write_csv(processed_crime_boston, "external_data/processed_crime_boston.csv")
}

# section: crime chicago https://www.chicago.gov/city/en/dataset/crime.html
download_file_crime_chicago <- function () {
}
process_file_crime_chicago <- function () {
  library(tidyverse)
  raw_crime_chicago <- read_csv("external_data/downloaded_crime_chicago.csv")

  crime_chicago_selected <- raw_crime_chicago %>%
    dplyr::select(Date, `Primary Type`, Latitude, Longitude)

  crime_chicago_selected$Date <- as.Date(substr(crime_chicago_selected$Date, start = 1, stop = 10), format = "%m/%d/%Y")

  crime_chicago_selected_2020 <- crime_chicago_selected %>%
    filter(Date >= "2020-01-01" & Date <= "2020-12-31")

  processed_crime_chicago <- crime_chicago_selected_2020 %>%
    dplyr::select(-Date) %>%
    rename(variable=`Primary Type`, latitude=Latitude, longitude=Longitude) %>%
    add_column(estimate=1, .before = "latitude") %>%
    mutate(variable=str_c("is_crime_chicago ", variable)) %>%
    drop_na()  # 210829 rows before dropping NA; 208468 rows after dropping; 2361 rows dropped

  write_csv(processed_crime_chicago, "external_data/processed_crime_chicago.csv")
}

## section: crime los_angeles https://data.lacity.org/Public-Safety/Crime-Data-from-2010-to-2019/63jg-8b9z
download_file_crime_los_angeles <- function () {
}
process_file_crime_los_angeles <- function () {
  library(tidyverse)
  raw_crime_los_angeles <- read_csv("external_data/downloaded_crime_los_angeles.csv", col_types = "ccccccccccccccccccccccccccdd")

  crime_los_angeles_selected <- raw_crime_los_angeles %>% dplyr::select(`Date Rptd`, `Crm Cd Desc`, LAT, LON)

  crime_los_angeles_selected$`Date Rptd` <- as.Date(substr(crime_los_angeles_selected$`Date Rptd`, start = 1, stop = 10), format = "%m/%d/%Y")

  crime_los_angeles_selected_2020 <- crime_los_angeles_selected %>%
    filter(`Date Rptd` >= "2020-01-01" & `Date Rptd` <= "2020-12-31")

  processed_crime_los_angeles <- crime_los_angeles_selected_2020 %>%
    dplyr::select(-`Date Rptd`) %>%
    rename(variable=`Crm Cd Desc`, latitude=LAT, longitude=LON) %>%
    add_column(estimate=1, .before = "latitude") %>%
    mutate(variable=str_c("is_crime_los_angeles ", variable)) %>%
    filter(latitude!=0 & longitude!=0)

  write_csv(processed_crime_los_angeles, "external_data/processed_crime_los_angeles.csv")
}


## section: airbnb https://www.kaggle.com/kritikseth/us-airbnb-open-data
download_file_airbnb <- function () {
}
process_file_airbnb <- function () {
  library(tidyverse)
  raw_airbnb <- read_csv("external_data/downloaded_airbnb.csv", col_types = "ccccccddccccccccc")

  airbnb_selected <- raw_airbnb %>%
    dplyr::select(room_type, latitude, longitude)

  processed_airbnb <- airbnb_selected %>%
    rename(variable=room_type) %>%
    add_column(estimate=1, .before = "latitude") %>%
    mutate(variable=str_c("is_airbnb ", variable))

  write_csv(processed_airbnb, "external_data/processed_airbnb.csv")
}


# section: ParkServe data https://www.tpl.org/parkserve/downloads
download_file_park <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  download.file(url = "https://parkserve.tpl.org/downloads/ParkServe_Shapefiles_05042022.zip?_ga=2.103216521.887440371.1664905337-1364699585.1664905337", destfile = "external_data/ParkServe_shp.zip")
}

#run file processing function
process_file_park <- function () {  # unzip the downloaded file and save the target data layer as csv file)
  unzip("external_data/ParkServe_shp.zip", exdir="external_data/ParkServe_shp")
}
#test 11-14:
shp_directory<-'external_data/ParkServe_shp/ParkServe_Shapefiles_05042022/ParkServe_Parks.shp'

shp_preprocess <- function (shp_directory){
  #"external_data/ParkServe_shp/ParkServe_Shapefiles_05042022/ParkServe_Parks.shp"
  park_shp <- st_read(shp_directory) 
  
  #Identify states
  states_sf <- st_transform(us_states( map_date = NULL, resolution = c("low", "high"), states = NULL), 4326)
  points_sf = st_as_sf(dataset_geocoded%>%filter(!is.na(lat) & !is.na(long)), coords = c("long", "lat"), crs = 4326, agr = "constant")
  states <- as.data.frame( st_join(points_sf, states_sf, join = st_intersects) ) %>% dplyr::select(name, -geometry)%>%unique()%>% drop_na()%>% as.list() 
  
  park_shp<-park_shp%>% filter(Park_State %in% states$name)
  park_shp<-st_transform(park_shp, crs=4326)
  
  #park_shp<-st_make_valid(park_shp) ## gives error
  park_shp<-st_make_valid(park_shp %>% filter(!is.na(st_is_valid(park_shp))))

}

## section: NLCD
post_process_nlcd<-function(variable_list, prop.nlcd){
  prop.nlcd<-data.frame(prop.nlcd)
if(nrow(prop.nlcd)==0){
prop.nlcd<-data.frame(x=NA, Freq=NA)
}  

  environmental_measures<-merge(variable_list, prop.nlcd, by=c('x'), all.x=TRUE)
  environmental_measures[is.na(environmental_measures)]<-0
return(environmental_measures)
}
  
## section: PLACES

download_file_places<-function() {
  #set the url for the dataset for the year of interest
  places2018url='https://chronicdata.cdc.gov/api/views/yjkw-uj5s/rows.csv?accessType=DOWNLOAD' #2021 release: 2018, 2019 data
  places2017url='https://chronicdata.cdc.gov/api/views/ib3w-k9rq/rows.csv?accessType=DOWNLOAD' #2020 release: 2018, 2017

  download.file(url=places2017url, destfile="external_data/downloaded_places2017.csv")
  download.file(url=places2018url, destfile="external_data/downloaded_places2018.csv")
}

process_file_places<-function(year=2017){
  #for(i in 1:length(years)){
  if(!is.na(year)){
  if(year==2017){raw_places<-read.csv('external_data/downloaded_places2017.csv')}
  if(year==2018){raw_places<-read.csv('external_data/downloaded_places2018.csv')}
  if(year>2017){
  raw_places<-read.csv('external_data/downloaded_places2018.csv')
    processed_dataframe<-raw_places %>%
      filter(StateAbbr %in% states$state_abbr) %>%
      rename(total_pop_2010=TotalPopulation) %>% #updated label to reflect that this is the total population based on 2010 census
      dplyr::select('TractFIPS', "total_pop_2010", "ACCESS2_CrudePrev", "ARTHRITIS_CrudePrev", "BINGE_CrudePrev", "BPHIGH_CrudePrev", "BPMED_CrudePrev", "CANCER_CrudePrev",   
                    "CASTHMA_CrudePrev","CERVICAL_CrudePrev", "CHD_CrudePrev", "CHECKUP_CrudePrev",   "CHOLSCREEN_CrudePrev","COLON_SCREEN_CrudePrev", "COPD_CrudePrev", "COREM_CrudePrev",       
                    "COREW_CrudePrev", "CSMOKING_CrudePrev", "DENTAL_CrudePrev", "DEPRESSION_CrudePrev","DIABETES_CrudePrev", "GHLTH_CrudePrev", "HIGHCHOL_CrudePrev", "KIDNEY_CrudePrev",      
                    "LPA_CrudePrev", "MAMMOUSE_CrudePrev", "MHLTH_CrudePrev", "OBESITY_CrudePrev","PHLTH_CrudePrev", "SLEEP_CrudePrev", "STROKE_CrudePrev", "TEETHLOST_CrudePrev")%>%
      melt(id='TractFIPS')%>%
      rename(GEOID=TractFIPS, estimate=value) %>%
      mutate(GEOID=ifelse(as.numeric(as.character(GEOID))<10000000000, as.character(paste0('0', as.character(GEOID), "")), as.character(GEOID)), 
             year=years[i])%>% #convert to GEOID to character for joining data, need to add an extra 0 in front for some values
      filter(!grepl('95CI', variable)) #remove the 95% CI for the estimates
  }
 if(year==2017){
  raw_places<-read.csv('external_data/downloaded_places2017.csv')
   processed_dataframe<-raw_places %>%
      filter(StateAbbr %in% states$state_abbr) %>%
      rename(total_pop_2010=TotalPopulation) %>% #updated label to reflect that this is the total population based on 2010 census
      mutate(DEPRESSION_CrudePrev=0, ## add blank rows for variables that are only available in 2021 release (Depression & General Health measures)
             GHLTH_CrudePrev=0) %>%
      dplyr::select('TractFIPS', "total_pop_2010", "ACCESS2_CrudePrev", "ARTHRITIS_CrudePrev", "BINGE_CrudePrev", "BPHIGH_CrudePrev", "BPMED_CrudePrev", "CANCER_CrudePrev",   
                    "CASTHMA_CrudePrev","CERVICAL_CrudePrev", "CHD_CrudePrev", "CHECKUP_CrudePrev",   "CHOLSCREEN_CrudePrev","COLON_SCREEN_CrudePrev", "COPD_CrudePrev", "COREM_CrudePrev",       
                    "COREW_CrudePrev", "CSMOKING_CrudePrev", "DENTAL_CrudePrev", "DEPRESSION_CrudePrev","DIABETES_CrudePrev", "GHLTH_CrudePrev", "HIGHCHOL_CrudePrev", "KIDNEY_CrudePrev",      
                    "LPA_CrudePrev", "MAMMOUSE_CrudePrev", "MHLTH_CrudePrev", "OBESITY_CrudePrev","PHLTH_CrudePrev", "SLEEP_CrudePrev", "STROKE_CrudePrev", "TEETHLOST_CrudePrev")%>%
      melt(id='TractFIPS')%>%
      rename(GEOID=TractFIPS, estimate=value) %>%
      mutate(GEOID=ifelse(as.numeric(as.character(GEOID))<10000000000, as.character(paste0('0', as.character(GEOID), "")), as.character(GEOID)), 
             year=year)%>% #convert to GEOID to character for joining data, need to add an extra 0 in front for some values
      ## add blank variables for Depression and GLHTH
      filter(!grepl('95CI', variable)) #remove the 95% CI for the estimates
  }   
  
  write_csv(processed_dataframe, '~/workspace/external_data/processed_places.csv')
  }
  
}

