#### Parkserve Data Settings -- Inspace ####
##set up data pull specs:
parks_vars<-c('park_proportion', 'park_distance')

names_of_variables_to_get<-parks_vars
first_var<-parks_vars[1]

parks_years<-c(2022)
parks_selected_years<-c(2022)

parks_description<-'The ParkServe dataset contains geographical information of parks in the US. 
We will download files and use to calculate distance to the nearest park (within the set buffer), and the percent of land in a given buffer that is park land. You can learn more at https://www.tpl.org/parkserve.
The first time you run this data pull, the file will be downloaded and processed in the background, which can take around 20 minutes. Note that if your firewall prevents you from downloading files, you may need to contact
Amy and have the file sent directly to you.'

download_file_park <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  download.file(url = "https://parkserve.tpl.org/downloads/ParkServe_Shapefiles_05042022.zip?_ga=2.103216521.887440371.1664905337-1364699585.1664905337", destfile = "external_data/ParkServe_shp.zip")
  #download.file(url = 'https://parkserve.tpl.org/downloads/ParkServe_Shapefiles_05152023.zip', destfile = "external_data/ParkServe_shp.zip")
  
  }

process_file_park <- function () {  # unzip the downloaded file and save the target data layer as csv file)
  unzip("external_data/ParkServe_shp.zip", exdir="external_data/ParkServe_shp")
}

prepare_park_data<-function() {
  if(file.exists("external_data/ParkServe_shp/ParkServe_Shapefiles_05042022/ParkServe_Parks.shp")==FALSE){
download_file_park()
process_file_park()
  }
  if(exists('park_shp')==FALSE){
    
park_shp <- shp_preprocess(shp_directory = "external_data/ParkServe_shp/ParkServe_Shapefiles_05042022/ParkServe_Parks.shp")

#park_shp <- shp_preprocess(shp_directory = 'external_data/ParkServe_shp/ParkServe_Shapefiles_05152023/ParkServe_Parks.shp')
}
  if(exists('park_shp')==TRUE){park_shp<-park_shp}
return(park_shp)  
}
