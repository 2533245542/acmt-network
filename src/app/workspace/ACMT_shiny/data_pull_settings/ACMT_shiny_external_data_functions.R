### loop functiosn for external data -- using the ACMT ####

## function to add a row to dataset for each year/radius ####
add_rows_columns<-function(dataset, variable_list, radius_vector, years){
  var.cols<-data.frame(matrix(nrow=nrow(dataset), ncol=length(variable_list)))#make a columns for each variable in the list
  colnames(var.cols)<-variable_list #name the columns
  dataset<-cbind(var.cols, dataset) #add the columns to the dataset
  for(year1 in 1:length(years)){
    dataset$year<-years[year1]
    for(radius1 in 1:length(radius_vector)){
      dataset$radius<-radius_vector[radius1]
      if(radius1==1){
        dataset.radius<-dataset
      }
      if(radius1>1){
        dataset.radius<-rbind(dataset.radius, dataset)
      }
    }
    
    if(year1==1){
      dataset.year<-dataset.radius
    }
    if(year1>1){
      dataset.year<-rbind(dataset.radius, dataset.year)
    }
  }
  return(dataset.year)
}

### loop function for pulling measures for each year/radius for a dataset ####

#run loop to pull variables
external_data_loop<-function(dataset=dataset, years=years, radius_vector=radius_vector, variable_list=variable_list){
  #dataset<-add_rows_columns(dataset=dataset, variable_list=variable_list, radius_vector=radius_vector, years=years) ##add rows and columns for each year/radius/variable
  #loop through each year
  for(year in 1:length(years)){
    year<-years[year]
    print(year) #print the year to track progress
    #loop through each radii
    for(radius in 1:length(radius_vector)){
      radius<-radius_vector[radius]
      print(radius) #print radii to track progress
      
      ## loop through each id to pull measures for each lat/long combination
      for(address in 1:nrow(dataset[dataset$radius==radius & dataset$year==year,])) {
        tryCatch({if(!is.na(dataset[dataset$radius==radius & dataset$year==year,][,1][address])) next #skip the row if the data is already there
          print(address) #print the number to keep track of progress
          latitude<-dataset[dataset$radius==radius & dataset$year==year,]$lat[address] #set lat
          longitude<-dataset[dataset$radius==radius & dataset$year==year,]$long[address] #set long
          
          ##pull measures for each lat/long  
          environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get = NULL, 
                                                          external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE) #pull measures for given lat & long
          #put the pulled values into the right columns/rows in the dataset
          for (name_of_variable in names_of_variables_to_get) {
            dataset[dataset$radius==radius & dataset$year==year,][[name_of_variable]][address] <- environmental_measures[environmental_measures$names == name_of_variable, ]$values 
            
          }},error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
      }}}
  return(dataset)
  }

## Create Functions for Park Proportion and Distance to park

get_distance_to_shapefile <- function(lat, long, radius_meters, shp_processed){
  park_shp <- shp_processed
  loc <- get_point_buffer_for_lat_long(long=long, lat=lat, radius_meters)
  area_intersect <- st_intersection(park_shp, loc)
  if (nrow(area_intersect) == 0){
    return(NA)
  }
  long <- c(long)
  lat <- c(lat)
  lonlat <- data.frame(cbind(long, lat))
  point = st_as_sf(lonlat, coords=c("long", "lat"), crs=4326)
  dist <- st_distance(point, area_intersect, by_element = TRUE)
  return(min(dist))
}

# The function to calculate the proportion of park area within the circle centered at (lat, long) point with radius = radius_meter
get_proportion_in_shapefile <- function(lat, long, radius_meters, shp_processed){
  park_shp <- shp_processed
  loc <- get_point_buffer_for_lat_long(long=long, lat=lat, radius_meters)
  area_intersect <- st_intersection(park_shp, loc)
  proportion <- sum(st_area(area_intersect))/st_area(loc)
  return(proportion)
}



