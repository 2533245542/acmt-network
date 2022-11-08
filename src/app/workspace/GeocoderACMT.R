library(httr)
library(tidyverse)
library(jsonlite)
library(sf)
library(tidycensus)
library(geosphere)
library(stringi)
library(dplyr)
library(units)
library(USAboundaries)
library(raster)
library(reshape2)
library(tigris)
library(lwgeom)

enable_connection_to_docker_network <- TRUE
path <- "http://host.docker.internal:5000/latlong?"

geocode <- function(address) {
  if (!enable_connection_to_docker_network) {
    stop("connection to docker network is disabled")
  }
  request <- GET(url = path, query=list(q=address))
  latitude <- NA
  longitude <- NA
  if (request$status_code > 300) {
    print(sprintf("status code for address %s is %s", address, request$status_code))
  } else {
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE)
    if (!is.null(df$rating)) { rating <- as.numeric(as.character(df$rating)) }
    if (!is.null(df$lat)) { latitude <- as.numeric(as.character(df$lat)) }
    if (!is.null(df$long)) { longitude <- as.numeric(as.character(df$long)) }
  }
  if (is.na(latitude) && is.na(longitude)) {
    stop("The state of this address is not loaded into database")
  }
  return(list(latitude=latitude, longitude=longitude, rating=rating))
}

if (!dir.exists("ACMT")) {
  dir.create("ACMT")
}

#download.file(url = "http://sandbox.idre.ucla.edu/mapshare/data/usa/other/spcszn83.zip", destfile = "ACMT/spcszn83.zip")
#unzip("ACMT/spcszn83.zip", exdir="ACMT")
#download.file(url = "https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_500k.zip", destfile = "ACMT/cb_2017_us_county_500k.zip")
#unzip("ACMT/cb_2017_us_county_500k.zip", exdir="ACMT")
acs_columns_url <- "http://host.docker.internal:7000/ACSColumns.csv"
if (enable_connection_to_docker_network) {
  download.file(url = acs_columns_url, destfile = "ACMT/ACSColumns.csv")
}

state_plane_zones <- sf::st_read(dsn="ACMT", layer="spcszn83")
counties <- sf::st_read(dsn="ACMT", layer="cb_2017_us_county_500k")
counties <- st_transform(counties, 4326)

state_proj$standard_zone <- state_proj$zone
state_proj$standard_zone[state_proj$zone == "west"] <- "W"
state_proj$standard_zone[state_proj$zone == "east"] <- "E"
state_proj$standard_zone[state_proj$zone == "central"] <- "C"
state_proj$standard_zone[state_proj$zone == "north"] <- "N"
state_proj$standard_zone[state_proj$zone == "south"] <- "S"
state_proj$standard_zone[state_proj$zone == "island"] <- "I"
state_proj$standard_zone[state_proj$zone == "mainland"] <- "M"
state_proj$standard_zone[state_proj$zone == "long island"] <- "LI"
state_proj$standard_zone[state_proj$zone == "north central"] <- "NC"
state_proj$standard_zone[state_proj$zone == "south central"] <- "SC"
state_proj$standard_zone[state_proj$zone == "east central"] <- "EC"
state_proj$standard_zone[state_proj$zone == "west central"] <- "WC"
state_proj$ZONE <- paste(state_proj$state, state_proj$standard_zone, sep="_")
state_proj$ZONE[is.na(state_proj$zone)] <- state_proj$state[is.na(state_proj$zone)]
state_proj$ZONE

get_projection_for_lat_long <- function(long, lat) {
  point <- st_sfc(st_point(c(long, lat)), crs=4326)
  state_plane_zones %>%
    filter(st_contains(state_plane_zones, point, sparse = F) == 1) %>%
    left_join(state_proj, by="ZONE") %>%
  {.} -> selected_zone
  if (nrow(selected_zone) == 0) {
    search_factor <- 1
    while (nrow(selected_zone) == 0) {
      point <- st_sfc(st_point(c(long+runif(1, -0.1*search_factor, 0.1*search_factor),
                                 lat+runif(1, -0.1*search_factor, 0.1*search_factor))), crs=4326)
      state_plane_zones %>%
        filter(st_contains(state_plane_zones, point, sparse = F) == 1) %>%
        left_join(state_proj, by="ZONE") %>%
      {.} -> selected_zone
      search_factor <- search_factor + 1
    }
  }
  return(selected_zone$proj4_string)
}
get_point_buffer_for_lat_long <- function(long, lat, radius_meters) {
  proj4_string <- get_projection_for_lat_long(long, lat)
  point <- st_sfc(st_point(c(long, lat)), crs=4326)
  point_projected <- st_transform(point, proj4_string)
  radius <- set_units(radius_meters, "meters")
  point_buffer <- st_buffer(point_projected, dist=radius)
  point_buffer <- st_transform(point_buffer, crs=4326)
  return(point_buffer)
}
get_travelable_buffer <- function (latitude=47.663, longitude=-122.333, travel_type="foot", travel_time=10) {
  if (!travel_type %in% c("car", "bike", "foot")) {stop("Travel type not supported")}
  travel_buffer <- adpated_osrmIsochrone(loc=c(longitude, latitude), osrm.profile=travel_type, breaks=travel_time, returnclass="sf")
  buffer <- travel_buffer$geometry  # correct projection, WGS84 is equivalent to 4326
  return(buffer)
}

state_list <- list()
get_geometries_of_a_county <- function(state, county, year=2017, geoid_type="Census Tract", use_lower_resolution_geo_data=FALSE) {
  if (!geoid_type %in% c("Census Tract", "Block Group")) {
    stop("Unsupported GEOID type")
  }
  print("called get_statecounty_tracts")
  if (as.numeric(state) < 0 || as.numeric(state) > 55) { message(sprintf("error!  Unknown state %s", state)) }
  if (as.numeric(county) < 0 || as.numeric(county) > 1000) { message(sprintf("error!  Unknown county %s", county)) }
  state_counties <- state_list[[state]]
  if (is.null(state_counties)) {
    state_counties <- list()
  }
  tracts_without_water <- state_counties[[county]]
  if (is.null(tracts_without_water)) {
    print(sprintf("Looking up tracts for state %s , county %s", state, county))

    tracts <- NULL
    if (geoid_type == "Census Tract") {
      tracts <- st_as_sf(tracts(state = state, county = county, year=year, cb=use_lower_resolution_geo_data))
    } else if (geoid_type == "Block Group") {
      tracts <- st_as_sf(block_groups(state = state, county = county, year=year, cb=use_lower_resolution_geo_data))
    } else {
      stop("Unsupported GEOID type")
    }

    # try to get water area for this year
    water <- NULL
    tryCatch({
      water <- st_union(st_as_sf(area_water(state = state, county = county, year=year)))
    }, error = function (condition) {
      if (condition$message == "area_water is not currently available for years prior to 2011.  To request this feature,\n                   file an issue at https://github.com/walkerke/tigris."){
        warning(paste0("Water area not available for years prior to 2011. The requested year is ", as.character(year)))
      } else {
        stop("Unknown error in getting water area")
      }
    })
    if (!is.null(water)) {
      tracts <- st_difference(tracts, water)  # if has water,  substract the water from tracts
    }

    state_counties[[county]] <- tracts
  }
  state_list[[state]] <- state_counties
  return(tracts)
}


get_acs_results_for_available_variables <- function (acs_var_names, state, county, year) {
  ### Remove the acs_var_names that are not available for the year and only return the available ones ###
  ### Return NA if error is not due to missing acs_var_names ###
  if (year < 2010 | year > 2021) {
    stop("Year must be in between 2010 and 2021, inclusive")
  }
  input_acs_var_names <- acs_var_names
  acs_results <- NA
  has_missing_variable_error <- TRUE
  while(has_missing_variable_error) {
    # Try to get acs results and if there is no error, we just proceed to the next step. If there is an error, we check if it is due to the missing variable; if it is, we remove the missing variable and repeat the loop; if it is not due to missing variable, we proceed to the next step. Thus, we proceed when there is no error or the error is not caused by missing variable; we continue the loop only when there is missing variable error.

    if (length(acs_var_names) == 0) {  # break when all variables are missing
      break
    }

    has_missing_variable_error <- tryCatch({  # note that codes in try is not inside a new function, just treat it as normal R code
      acs_results <- get_acs("tract", variables=acs_var_names, state=state, county=county, cache_table = T, census_api_key=CENSUS_API_KEY, geometry = F, keep_geo_vars = T, year=year)
      FALSE   # FALSE will be assigned to has_missing_variable_error; cannot use return here, otherwise the rest of the function will not execute
    }, error = function(condition) {  # note that in error handler, the codes are inside a new function; this is why we need <<- for assigning acs_var_names
      error_is_caused_by_missing_variable <- grepl(pattern = "Your API call has errors.  The API message returned is error: error: unknown variable '", x=condition$message, fixed = TRUE)
      error_is_caused_by_occational_request_failure <- grepl(pattern = "Your API call has errors.  The API message returned is <html><head><title>Error report</title></head><body><h1>HTTP Status 404 ", x=condition$message, fixed = TRUE) || grepl(pattern = "incorrect number of dimensions", x=condition$message, fixed = TRUE)
      if (error_is_caused_by_missing_variable) {  # remove the missing variable. Note that only one missing variable will be found by per get_acs run.
        front_trimmed_error_message <- sub("Your API call has errors.  The API message returned is error: error: unknown variable '", "", condition$message) # remove the former part
        code_of_the_missing_variable <- sub("E'.", "", front_trimmed_error_message)  # remove the latter part
        acs_var_names <<- acs_var_names[acs_var_names != code_of_the_missing_variable]
        print(paste("Removing missing variable:", code_of_the_missing_variable))
        return(TRUE)
      } else if (error_is_caused_by_occational_request_failure) {
        print("Occational error occurred when sending get_acs request. Trying again")
        return(TRUE)
      } else {
        print(acs_var_names)
        print(condition$message)
        stop("Unknown error occurred")
      }
    })
  }

  # when there are missing variables, warn what they are, and even stop
  if (length(input_acs_var_names) > length(acs_var_names)) {
    warning(sprintf("Some ACS variables are missing for year %s", year))
    print("The missing ACS variables are:")
    print(setdiff(input_acs_var_names, acs_var_names))  # set minus latter
    print("The non-missing ACS variables are:")
    print(acs_var_names)
    if (length(acs_var_names) == 0) {
      stop(sprintf("All ACS variables are missing for year %s", year))
    }
  }

  return(acs_results)
}
#### old interpolation code ####
#acs_variable_name_to_interpolate_by_sum_boolean_mapping <- c( TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
#TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

#names(acs_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("B23001_009", "B23001_016", "B23001_023", "B23001_030", "B23001_037", "B23001_044", "B23001_051", "B23001_058", "B23001_065", "B23001_072", "B23001_077", "B23001_082", "B23001_087", "B23001_002", 
#"B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014", 
#"B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019", "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", "B01001_026", "B01001_027", "B01001_028", 
#"B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", "B01001_042", 
#"B01001_043", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049", "B01001H_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", 
#"B02001_008", "B03002_001", "B03002_004", "B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006", "B05012_002", "B05012_003", "B06008_001", "B06008_002", "B06008_003", "B06008_004", 
#"B06008_005", "B06008_006", "B06008_007", "B06008_013", "B06009_001", "B06009_002", "B06012_001", "B06012_002", "B06012_003", "B07201_002", "B07201_004", "B07201_014", "B08006_001", "B08006_003", 
#"B08006_004", "B08006_008", "B08006_014", "B08006_015", "B08006_016", "B08006_017", "B08302_002", "B08302_003", "B08302_004", "B08302_005", "B08302_006", "B08302_007", "B08302_008", "B08302_009", 
#"B08302_010", "B08302_011", "B08302_012", "B08302_013", "B08302_014", "B08302_015", "B11012_001", "B11012_009", "B11012_010", "B11012_014", "B11012_015", "B15002_002", "B15002_011", "B15002_015", 
#"B15002_019", "B15002_028", "B15002_032", "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", 
#"B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025", 
#"B16005_001", "B16005_004", "B16005_007", "B16005_008", "B16005_009", "B16005_012", "B16005_013", "B16005_014", "B16005_017", "B16005_018", "B16005_019", "B16005_022", "B16005_023", "B16005_024", 
#"B16005_026", "B16005_029", "B16005_030", "B16005_031", "B16005_034", "B16005_035", "B16005_036", "B16005_039", "B16005_040", "B16005_041", "B16005_044", "B16005_045", "B17001_001", "B17001_002", 
#"B17012_001", "B17012_002", "B17023_001", "B17023_002", "B18101_001", "B18101_002", "B18101_003", "B18101_004", "B18101_006", "B18101_007", "B18101_009", "B18101_010", "B18101_012", "B18101_013", 
#"B18101_015", "B18101_016", "B18101_018", "B18101_019", "B18101_021", "B18101_022", "B18101_023", "B18101_025", "B18101_026", "B18101_028", "B18101_029", "B18101_031", "B18101_032", "B18101_034", 
#"B18101_035", "B18101_037", "B18101_038", "B19001_001", "B19001_002", "B19001_003", "B19001_004", "B19001_005", "B19001_006", "B19001_011", "B19001_012", "B19001_013", "B19001_014", "B19001_015", 
#"B19001_016", "B19001_017", "B19013_001", "B19054_001", "B19054_002", "B19058_001", "B19058_002", "B23025_001", "B23025_005", "B25001_001", "B25002_001", "B25002_003", "B25003_001", "B25003_002", 
#"B25014_001", "B25014_002", "B25014_005", "B25014_006", "B25014_007", "B25014_008", "B25014_011", "B25014_012", "B25014_013", "B25016_001", "B25016_007", "B25016_016", "B25024_001", "B25024_007", 
#"B25024_008", "B25024_009", "B25024_010", "B25038_002", "B25038_007", "B25038_008", "B25038_009", "B25038_014", "B25038_015", "B25043_001", "B25043_002", "B25043_007", "B25043_011", "B25043_016", 
#"B25044_001", "B25044_002", "B25044_003", "B25044_009", "B25044_010", "B25064_001", "B25070_001", "B25070_010", "B25077_001", "B25088_002", "B25091_001", "B25091_002", "B25091_011", "B25091_013", 
#"B25091_022", "B25129_001", "B25129_002", "B25129_003", "B25129_038", "B25129_039", "B26001_001", "C24030_002", "C24030_018", "C24030_019", "C24030_029", "C24030_045", "C24030_046", "C24060_001", 
#"C24060_002", "B11003_016", "B11003_015", "B11003_009", "B11003_010", "B11003_001", "B19301_001", "B11004_010", "B11004_016", "B11004_001", "B11010_001", "B11010_003", "B11010_010", "B11011_001", 
#'B15002_001', 'B15002_003', 'B15002_004', 
#'B15002_005', 'B15002_006', 'B15002_007', 'B15002_008', 'B15002_009', 'B15002_010', 'B15002_011', 'B15002_012', 'B15002_013', 'B15002_014', 'B15002_016', 'B15002_017', 'B15002_018', 'B15002_019', 
#'B15002_020', 'B15002_021', 'B15002_022', 'B15002_023', 'B15002_024', 'B15002_025', 'B15002_026', 'B15002_027', 'B15002_028', 'B15002_029', 'B15002_030', 'B15002_031', 'B15002_032', 'B15002_033', 'B15002_034', 'B15002_35') 


#### ####
get_count_variable_for_lat_long <- function(long, lat, radius_meters, acs_var_names=NULL, year=year, external_data=NULL, geoid_type = "Census Tract", fill_missing_GEOID_with_zero=FALSE, use_lower_resolution_geo_data=FALSE, variable_name_to_interpolate_by_sum_boolean_mapping=NULL, return_point_estimate=FALSE, custom_buffer=NULL) {  # count_results might not have the variable measures for all GEOIDs in census tracts, in that case, use 0 for the measure; if this is not done, the returned result will be NA

  if (is.null(variable_name_to_interpolate_by_sum_boolean_mapping)) {
    stop("Function get_count_variable_for_lat_long is not provided with variable_name_to_interpolate_by_sum_boolean_mapping")
  }

  # check for if asked to handle external data
  using_external_data <- FALSE
  names_of_interested_variables <- acs_var_names   # variable names we want to output
  if (is.null(acs_var_names) && is.null(year) && !is.null(external_data)) {  # when no acs_var_names, year, but have external_data
    using_external_data <- TRUE
    names_of_interested_variables <- unique(external_data$variable)
  }

  # find counties intersecting the point buffer
  if(is.null(custom_buffer)) {
    point_buffer <- get_point_buffer_for_lat_long(long=long, lat=lat, radius_meters=radius_meters)
  } else {
    point_buffer <- custom_buffer
  }
  if(return_point_estimate&&!is.null(custom_buffer)) {
    stop("Please either get measure for the custom buffer or get point estiamte for a lat/long")
  }
  index_of_intersecting_counties <- st_intersects(point_buffer, counties)
  if (return_point_estimate) {  # TODO return_point_estimate: might not need this. Speed up by just looking for one county
    point_buffer <- get_point_buffer_for_lat_long(long, lat, 1)
    index_of_intersecting_counties <- st_intersects(point_buffer, counties)
    stopifnot("When asking for point estimate, only the lat/long will be used for locating the county, thus there should be one intersecting county only" = length(index_of_intersecting_counties)==1)
  }
  if (length(index_of_intersecting_counties) < 1) {
    message("get_count_variable_for_lat_long error: buffer does not overlap US counties")
  }
  intersecting_counties_fips <- unique(as.character(counties$GEOID[index_of_intersecting_counties[[1]]]))
  print(intersecting_counties_fips)
  intersecting_counties_fips_state_codes <- substr(intersecting_counties_fips, 1, 2)
  intersecting_counties_fips_county_codes <- substr(intersecting_counties_fips, 3, 5)

  # get measures and geometries to prepare for interpolatation
  geoid_and_columns_of_variable_value_to_geometry_dataframe_list <- list()
  for (i in seq_along(intersecting_counties_fips)) {
    geoid_to_geometry_dataframe <- get_geometries_of_a_county(state=intersecting_counties_fips_state_codes[i], county=intersecting_counties_fips_county_codes[i], geoid_type=geoid_type, use_lower_resolution_geo_data=use_lower_resolution_geo_data)

    geoid_to_variable_name_to_variable_value_dataframe <- NA

    if (using_external_data) {
      geoid_to_variable_name_to_variable_value_dataframe <- external_data
    } else {
      # Census API throws intermittent errors with old years.  Add a retry mechanism to try to track it down
      tries <- 0
      while (length(geoid_to_variable_name_to_variable_value_dataframe) == 1 && is.na(geoid_to_variable_name_to_variable_value_dataframe) && tries < 10) {  # the first condition ensures the loop breaks without executing is.na (such that no warning is made)
        tries <- tries + 1
        try(
          geoid_to_variable_name_to_variable_value_dataframe <- get_acs_results_for_available_variables(
            acs_var_names=names_of_interested_variables,
            state=intersecting_counties_fips_state_codes[i],
            county=intersecting_counties_fips_county_codes[i],
            year=year)
        )
      }
      if (length(unique(geoid_to_variable_name_to_variable_value_dataframe$variable)) < length(names_of_interested_variables)) {   # if missing variables were pruned, update names_of_interested_variables to let it only include the available variables
        names_of_interested_variables <- names_of_interested_variables[names_of_interested_variables %in% geoid_to_variable_name_to_variable_value_dataframe$variable]  # not assigning acs_results$variable directly to names_of_interested_variables because although they are the same, the order of variables is different due to calling get_acs; in short, we want to keep the order of the variables to pass tests
      }
    }

    geoid_to_variable_name_to_variable_value_dataframe$estimate[is.na(geoid_to_variable_name_to_variable_value_dataframe$estimate)] <- 0
    geoid_to_columns_of_variable_value_dataframe <- dcast(geoid_to_variable_name_to_variable_value_dataframe, GEOID ~ variable, value.var="estimate" )

    columns_of_variable_value_to_geometry_dataframe <- left_join(x=geoid_to_geometry_dataframe, y=geoid_to_columns_of_variable_value_dataframe, by="GEOID")  # GEOID is used for matching features to geometries; thus in ACMT, only the GEOID between census tract and features should match
    #columns_of_variable_value_to_geometry_dataframe_list[[i]]  <- columns_of_variable_value_to_geometry_dataframe[, names_of_interested_variables]
    geoid_and_columns_of_variable_value_to_geometry_dataframe_list[[i]]  <- columns_of_variable_value_to_geometry_dataframe[, c("GEOID", names_of_interested_variables)] # TODO include geoid
  }

  if (length(geoid_and_columns_of_variable_value_to_geometry_dataframe_list) < 1) {
    message("get_count_variable_for_lat_long: No block group data returned from census")
  }
  # debug start TODO just to introduce a version of dataframe that contains geoid
  all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe <- do.call(rbind, geoid_and_columns_of_variable_value_to_geometry_dataframe_list)
  all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe <- st_transform(all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe, 4326)
  all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe[, names_of_interested_variables]
  # debug end

  #all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- do.call(rbind, columns_of_variable_value_to_geometry_dataframe_list)
  #all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- st_transform(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe, 4326)
  if (fill_missing_GEOID_with_zero) {
    all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[is.na(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe)] <- 0
  }

  for (variable_name in names_of_interested_variables) {
    if (is.na(variable_name_to_interpolate_by_sum_boolean_mapping[variable_name])) {
      stop(paste("Interploation by sum boolean for", variable_name, "does not exist."))
    }
  }

  # interpolate measures from the point buffer
  interpolate_one_varaible <- function(variable_name, variable_name_to_interpolate_by_sum_boolean_mapping) {
    suppressWarnings(st_interpolate_aw(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[, variable_name], point_buffer, extensive=variable_name_to_interpolate_by_sum_boolean_mapping[variable_name])[[variable_name]]) # e.g. population is extensive, population density is non-extensive (intensive)
  }
  values_of_interested_variables <- lapply(names_of_interested_variables, FUN = interpolate_one_varaible, variable_name_to_interpolate_by_sum_boolean_mapping)
  if (return_point_estimate) {  # TODO overwrite values_of_interested_variables
    # get geoid for the lat/long
    geoid_for_lat_long <- get_geoid_for_lat_long_annonymous(lat = lat, lon = long, geoid_type = geoid_type)
    # get variable_to_value_dataframe for the GEOID
    variable_to_value_dataframe_for_the_geoid <- all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe %>%
      filter(GEOID==geoid_for_lat_long) %>%
      st_drop_geometry() %>%
      dplyr::select(-GEOID)
    values_of_interested_variables_for_the_geoid <- vector("list", length(names_of_interested_variables))
    names(values_of_interested_variables_for_the_geoid) <- names_of_interested_variables
    # extract the values and re-assign values_of_interested_variables
    for (name_of_variable in names_of_interested_variables) {
      values_of_interested_variables_for_the_geoid[[name_of_variable]] <- variable_to_value_dataframe_for_the_geoid[[name_of_variable]]
    }
    values_of_interested_variables <- values_of_interested_variables_for_the_geoid
  }
  return(data.frame(name=names_of_interested_variables, estimate=unlist(values_of_interested_variables)))
}

get_acs_standard_columns <- function(year=2017, codes_of_acs_variables_to_get=NA, set_var_list=FALSE) {
  # To do: cache this
### import acs list according to year of interest
  print(year)
  if(set_var_list==TRUE){
    print("Reading set ACS column list")
      acs_columns<-read.csv('ACMT/ACSColumns.csv')
  }
  if(set_var_list==FALSE){  print('Reading year-specific ACS column list')
  if(year==2010){
    acs_columns<-read.csv('ACMT/2010ACSColumns.csv')
  }
  if(year==2011){
    acs_columns<-read.csv('ACMT/ACSColumns2011.csv')
  }
  if(year>2011 & year<2019){
    acs_columns<-read.csv('ACMT/ACSColumns2012_thru_18.csv')
  }
  if(year==2019){
  acs_columns <- read.csv("ACMT/ACSColumns2019.csv")
  }
  if(year==2020){
  acs_columns<-read.csv('ACMT/ACSColumns2020.csv')
  }
if(!is.null(codes_of_acs_variables_to_get)){
  if (!is.na(codes_of_acs_variables_to_get[1])) {  # filter acs_columns by the provided variables
    acs_columns <- acs_columns[acs_columns$acs_col %in% codes_of_acs_variables_to_get, ]
  }
}
  }
  acs_varnames <- acs_columns$acs_col   # in fact ACS variable codes
  
  ##set interpolation
  acs_variable_name_to_interpolate_by_sum_boolean_mapping<-acs_columns$acs_variable_name_to_interpolate_by_sum_boolean_mapping
  names(acs_variable_name_to_interpolate_by_sum_boolean_mapping)<-acs_columns$acs_col
 
  acs_count_names <- paste(acs_columns$var_name, "count", sep="_")  # all variables have counts
  if (length(acs_columns$var_name[acs_columns$universe_col != ""]) == 0) {   # prevent having something that is exactly "_proportion"
    acs_proportion_names <- character(0)
  } else {
    acs_proportion_names <- paste(acs_columns$var_name[acs_columns$universe_col != ""], "proportion", sep="_")   # only non-universal variables have proportions
  }
  acs_count_pretty_names <- acs_columns$pretty_name_count  # all have pretty name for count
  acs_proportion_pretty_names <- acs_columns$pretty_name_proportion[acs_columns$universe_col != ""]  # only non-universal varaibles have pretty name for proportion
  all_var_cols <- c(as.character(acs_columns$acs_col), as.character(acs_columns$universe_col))  #  acs_columns$acs_col is a superset of acs_columns$universe_col
  unique_var_cols <- unique(all_var_cols)
  unique_var_cols <- unique_var_cols[unique_var_cols != ""]  # all variable codes
  return(list(acs_proportion_names=acs_proportion_names,
              acs_count_names=acs_count_names,
              acs_unique_var_cols=unique_var_cols,
              acs_columns=acs_columns,
              acs_proportion_pretty_name_map=data.frame(acs_proportion_names, acs_proportion_pretty_names),
              acs_count_pretty_name_map=data.frame(acs_count_names, acs_count_pretty_names)))
}


get_acmt_standard_array <- function(lat, long, radius_meters, year=2017, codes_of_acs_variables_to_get=NA, external_data_name_to_info_list=NULL, fill_missing_GEOID_with_zero=FALSE, use_lower_resolution_geo_data=TRUE, return_point_estimate=FALSE, custom_buffer=NULL, set_var_list=FALSE
                                    ) {
  # check input validity
  if (year < 2010 | year > 2021) {stop("Year must be in between 2010 and 2021 (inclusive)")}
  if (is.na(long) | is.na(lat)) {stop("Null lat or long passed to get_acmt_standard_array")}

  # interpolate ACS dataset measures
  if(!is.null(codes_of_acs_variables_to_get)){ ###updated to only run if acs_columns is not NULL
  acs_info <- get_acs_standard_columns(year=year, codes_of_acs_variables_to_get=codes_of_acs_variables_to_get, set_var_list=set_var_list)
  acs_columns <- acs_info$acs_columns
  acs_proportion_names <- acs_info$acs_proportion_names
  acs_count_names <- acs_info$acs_count_names
  acs_unique_var_cols <- acs_info$acs_unique_var_cols
  
  ##Set interpolation designations (from acs_columns dataframe) ##
  acs_variable_name_to_interpolate_by_sum_boolean_mapping<-acs_columns$acs_variable_name_to_interpolate_by_sum_boolean_mapping
  names(acs_variable_name_to_interpolate_by_sum_boolean_mapping)<-as.character(acs_columns$acs_col)

    acs_count_results <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters, acs_var_names=acs_unique_var_cols, year=year, fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero, use_lower_resolution_geo_data=use_lower_resolution_geo_data, variable_name_to_interpolate_by_sum_boolean_mapping=acs_variable_name_to_interpolate_by_sum_boolean_mapping, return_point_estimate=return_point_estimate, custom_buffer=custom_buffer)

  # compute proportional measures for ACS dataset
  acs_unique_var_cols_contains_missing_variables <- (length(acs_count_results$name) < length(acs_unique_var_cols))
  if (acs_unique_var_cols_contains_missing_variables) {  # get_acs_standard_columns on only the non-missing variables
    acs_info <- get_acs_standard_columns(year=year, codes_of_acs_variables_to_get=acs_count_results$name)
    acs_columns <- acs_info$acs_columns
    acs_proportion_names <- acs_info$acs_proportion_names
    acs_count_names <- acs_info$acs_count_names
    acs_unique_var_cols <- acs_info$acs_unique_var_cols
    print(acs_varnames)
    names(acs_varnames) <- acs_columns$var_name
  }
  proportion_vals <- vector(length=length(acs_proportion_names))
  for (i in seq_along(proportion_vals)) {
    print(sprintf("Estimate for %s, which is %s will be divided by estimate for %s, which is %s", acs_columns$acs_col[acs_columns$universe_col != ""][i], acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i]))], acs_columns$universe_col[acs_columns$universe_col != ""][i], acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i]))] ))
    proportion_vals[i] <- acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i]))]/ acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i]))]
  }
  count_vals <- vector(length=length(acs_count_names))
  for (i in seq_along(count_vals)) {
    count_vals[i] <- acs_count_results$estimate[acs_count_results$name == as.character(acs_columns$acs_col[i])]
  }
  context_measurement_dataframe <- data.frame(names=c(acs_proportion_names, acs_count_names), values=c(proportion_vals, count_vals)) # combine count and proportional ACS measures
  }
  
  #make a blank dataframe if no acs variables were pulled
  if(is.null(codes_of_acs_variables_to_get)){
    context_measurement_dataframe<-data.frame(matrix(nrow=0, ncol=2)) 
    colnames(context_measurement_dataframe)<-c('names', 'values')
  }
  
  # interpolate external dataset measures
  if(!is.null(external_data_name_to_info_list)) {
    external_data_list <- load_external_data(external_data_name_to_info_list)

    variable_to_value_dataframe_list <- list()
    for (external_data_name in names(external_data_name_to_info_list)) {
      external_data_weighted_over_point_buffer_dataframe <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters, acs_var_names=NULL, year=NULL, external_data=external_data_list[[external_data_name]], geoid_type = external_data_name_to_info_list[[external_data_name]]$geoid_type, fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero, use_lower_resolution_geo_data=use_lower_resolution_geo_data, variable_name_to_interpolate_by_sum_boolean_mapping=external_data_name_to_info_list[[external_data_name]]$variable_name_to_interpolate_by_sum_boolean_mapping, return_point_estimate=return_point_estimate, custom_buffer=custom_buffer)
      variable_to_value_dataframe <- data.frame(names=external_data_weighted_over_point_buffer_dataframe$name, values=external_data_weighted_over_point_buffer_dataframe$estimate)
      variable_to_value_dataframe_list[[external_data_name]] <- variable_to_value_dataframe
    }
    external_data_measurement_dataframe <- do.call(rbind, variable_to_value_dataframe_list)

    
    context_measurement_dataframe <- rbind(context_measurement_dataframe, external_data_measurement_dataframe)
  }

  return(context_measurement_dataframe)
}
