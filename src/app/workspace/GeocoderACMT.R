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


source("external_data-file_downloader_and_processor.R")
source("external_data-file_loader.R")
source("external_data-variable_name_to_interpolation_type_mapping.R")

path <- "http://host.docker.internal:5000/latlong?"

geocode <- function(address) {
  request <- GET(url = path, query=list(q=address))
  latitude <- NA
  longitude <- NA
  if (request$status_code > 300) {
    print(sprintf("status code for address %s is %s", address, request$status_code))
  } else {
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE)
    if (!is.null(df$lat)) { latitude <- as.numeric(as.character(df$lat)) }
    if (!is.null(df$long)) { longitude <- as.numeric(as.character(df$long)) }
  }
  if (is.na(latitude) && is.na(longitude)) {
    stop("The state of this address is not loaded into database")
  }
  return(list(latitude=latitude, longitude=longitude))
}

if (!dir.exists("ACMT")) {
  dir.create("ACMT")
}

download.file(url = "http://sandbox.idre.ucla.edu/mapshare/data/usa/other/spcszn83.zip", destfile = "ACMT/spcszn83.zip")
unzip("ACMT/spcszn83.zip", exdir="ACMT")
download.file(url = "https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_500k.zip", destfile = "ACMT/cb_2017_us_county_500k.zip")
unzip("ACMT/cb_2017_us_county_500k.zip", exdir="ACMT")
acs_columns_url <- "http://host.docker.internal:7000/ACSColumns.csv"
download.file(url = acs_columns_url, destfile = "ACMT/ACSColumns.csv")

state_plane_zones <- sf::st_read(dsn="ACMT", layer="spcszn83")
counties <- sf::st_read(dsn="ACMT", layer="cb_2017_us_county_500k")
counties <- st_transform(counties, 4326)

get_state_from_fips <- function(fips_code) {
  if (is.null(fips_code)) {
    message("get_state_from_fips error: fips_code is null")
  }
  if (any(stri_length(fips_code) != 12)) {
    message("get_state_from_fips error: fips_code should be 12 characters")
  }
  return(substr(fips_code, 1, 2))
}

get_county_from_fips <- function(fips_code) {
  if (is.null(fips_code)) {
    message("get_state_from_fips error: fips_code is null")
  }
  if (any(stri_length(fips_code) != 12)) {
    message("get_state_from_fips error: fips_code should be 12 characters")
  }
  return(substr(fips_code, 3, 5))
}


get_statecounty_from_fips <- function(fips_code) {
  if (is.null(fips_code)) {
    message("get_statecounty_from_fips error: fips_code is null")
  }
  if (any(stri_length(fips_code) != 12)) {
    message("get_statecounty_from_fips error: fips_code should be 12 characters")
  }
  return(substr(fips_code, 1, 5))
}

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
  point_buffer <- st_buffer(point_projected,
                            dist=radius)
  point_buffer <- st_transform(point_buffer, crs=4326)
  return(point_buffer)
}

# Will need to project?
show_map_for_lat_long <- function(long, lat, radius_meters, projection=NULL) {
  point_buffer <- get_point_buffer_for_lat_long(long, lat, radius_meters)
  intersects <- st_intersects(point_buffer, block_groups)
  if (length(intersects) < 1) {
    message("show_map_for_lat_long error: buffer does not overlap US block groups")
  }
  print(intersects)
  overlapping_blocks <- block_groups[intersects[[1]],]
  if (!is.null(projection)) {
    print("projecting")
    overlapping_blocks <- st_transform(overlapping_blocks, projection)
    point_buffer <- st_transform(point_buffer, projection)
  }
  tm <- tm_shape(overlapping_blocks) +
    tm_fill(alpha=0.4, col="red") +
    tm_borders(col="black") +
    tm_shape(point_buffer) +
    tm_polygons(col="blue", alpha=0.7)
  return(tm)
}

state_list <- list()
get_statecounty_tracts <- function(state, county, year=2017, geoid_type="Census Tract", use_lower_resolution_geo_data=FALSE) {
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

old_statecounty_tracts <- function(state, county) {
  if (state != "53") { message("error! Not implemented outside WA State yet")}
  if (county == "033") { return (kc_tracts_without_water) }
  else if (county == "061") { return (sno_tracts_without_water) }
  else if (county == "053") { return (pierce_tracts_without_water) }
  else if (county == "057") { return (skagit_tracts_without_water) }
  else { message(sprintf("Error! County tract caching not implemented yet -- county code is %s", county)) }
}

get_acs_results_for_available_variables <- function (acs_var_names, state, county, year) {
  ### Remove the acs_var_names that are not available for the year and only return the available ones ###
  ### Return NA if error is not due to missing acs_var_names ###
  if (year < 2010 | year > 2019) {
    stop("Year must be in between 2010 and 2019, inclusive")
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

acs_variable_name_to_interpolate_by_sum_boolean_mapping <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
names(acs_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_026", "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B25001_001", "B05012_002", "B05012_003", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008", "B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006", "B06008_001", "B06008_002", "B06008_003", "B06008_004", "B06008_005", "B06008_006", "B06008_007", "B06008_013", "B07201_002", "B07201_004", "B07201_014", "B08006_001", "B08006_003", "B08006_004", "B08006_008", "B08006_014", "B08006_015", "B08006_016", "B08006_017", "B08302_002", "B08302_003", "B08302_004", "B08302_005", "B08302_006", "B08302_007", "B08302_008", "B08302_009", "B08302_010", "B08302_011", "B08302_012", "B08302_013", "B08302_014", "B08302_015", "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025", "B15002_002", "B15002_011", "B15002_015", "B15002_019", "B15002_028", "B15002_032")

get_count_variable_for_lat_long <- function(long, lat, radius_meters, acs_var_names=NULL, year=year, external_data=NULL, geoid_type = "Census Tract",fill_missing_GEOID_with_zero=FALSE, include_land_and_water_area=FALSE, use_lower_resolution_geo_data=FALSE, variable_name_to_interpolate_by_sum_boolean_mapping=NULL) {  # count_results might not have the variable measures for all GEOIDs in census tracts, in that case, use 0 for the measure; if this is not done, the returned result will be NA

  if (is.null(variable_name_to_interpolate_by_sum_boolean_mapping)) {
    stop("Function get_count_variable_for_lat_long is not provided with variable_name_to_interpolate_by_sum_boolean_mapping")
  }

  using_external_data <- FALSE
  names_of_interested_variables <- acs_var_names   # variable names we want to output

  if (is.null(acs_var_names) && is.null(year) && !is.null(external_data)) {  # when no acs_var_names, year, but have external_data
    using_external_data <- TRUE
    names_of_interested_variables <- unique(external_data$variable)
  }

  point_buffer <- get_point_buffer_for_lat_long(long, lat, radius_meters)

  index_of_intersecting_counties <- st_intersects(point_buffer, counties)
  if (length(index_of_intersecting_counties) < 1) {
    message("get_count_variable_for_lat_long error: buffer does not overlap US counties")
  }

  intersecting_counties_fips <- unique(as.character(counties$GEOID[index_of_intersecting_counties[[1]]]))
  print(intersecting_counties_fips)

  intersecting_counties_fips_state_codes <- substr(intersecting_counties_fips, 1, 2)
  intersecting_counties_fips_county_codes <- substr(intersecting_counties_fips, 3, 5)

  columns_of_variable_value_to_geometry_dataframe_list <- list()
  for (i in seq_along(intersecting_counties_fips)) {
    geoid_to_geometry_dataframe <- get_statecounty_tracts(state=intersecting_counties_fips_state_codes[i], county=intersecting_counties_fips_county_codes[i], geoid_type=geoid_type, use_lower_resolution_geo_data=use_lower_resolution_geo_data)

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
    columns_of_variable_value_to_geometry_dataframe_list[[i]]  <- columns_of_variable_value_to_geometry_dataframe[, names_of_interested_variables]
    if(include_land_and_water_area && !using_external_data){  # can only include land and water during retreving ACS vars; in other words, land and water only comes with ACS
      columns_of_variable_value_to_geometry_dataframe_list[[i]]  <- columns_of_variable_value_to_geometry_dataframe[, c("ALAND", "AWATER", names_of_interested_variables)]  # TODO
    }
  }

  if(include_land_and_water_area && !using_external_data) {
    names_of_interested_variables <- c("ALAND", "AWATER", names_of_interested_variables)  # TODO
  }

  if (length(columns_of_variable_value_to_geometry_dataframe_list) < 1) {
    message("get_count_variable_for_lat_long: No block group data returned from census")
  }
  all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- do.call(rbind, columns_of_variable_value_to_geometry_dataframe_list)
  all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- st_transform(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe, 4326)
  if (fill_missing_GEOID_with_zero) {
    all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[is.na(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe)] <- 0
  }

  for (variable_name in names_of_interested_variables) {
    if (is.na(variable_name_to_interpolate_by_sum_boolean_mapping[variable_name])) {
      stop(paste("Interploation by sum boolean for", variable_name, "does not exist."))
    }
  }

  # calculated weighted variable values for the point buffer
  #values_of_interested_variables <- mapply(FUN = function(variable_name, interpolate_by_sum_boolean) { suppressWarnings(st_interpolate_aw(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[, variable_name], point_buffer, extensive=interpolate_by_sum_boolean)[[variable_name]])}, names_of_interested_variables, variable_name_to_interpolate_by_sum_boolean_mapping, SIMPLIFY = TRUE)
  #return(data.frame(name=names_of_interested_variables, estimate=unname(values_of_interested_variables)))
  interpolate_one_varaible <- function(variable_name, variable_name_to_interpolate_by_sum_boolean_mapping) {
    suppressWarnings(st_interpolate_aw(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[, variable_name], point_buffer, extensive=variable_name_to_interpolate_by_sum_boolean_mapping[variable_name])[[variable_name]])
  }
  values_of_interested_variables <- lapply(names_of_interested_variables, FUN = interpolate_one_varaible, variable_name_to_interpolate_by_sum_boolean_mapping)
  return(data.frame(name=names_of_interested_variables, estimate=unlist(values_of_interested_variables)))
}

get_acs_standard_columns <- function(year=2017, codes_of_acs_variables_to_get=NA) {
  # To do: cache this
  print("Read ACS columns")
  acs_columns <- read.csv("ACMT/ACSColumns.csv")

  if (!is.na(codes_of_acs_variables_to_get[1])) {  # filter acs_columns by the provided variables
    acs_columns <- acs_columns[acs_columns$acs_col %in% codes_of_acs_variables_to_get, ]
  }

  acs_varnames <- acs_columns$acs_col   # in fact ACS variable codes
  print(acs_varnames)
  names(acs_varnames) <- acs_columns$var_name

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

get_acmt_standard_array <- function(long, lat, radius_meters, year=2017, codes_of_acs_variables_to_get=NA, external_data_name_to_info_list=NULL, fill_missing_GEOID_with_zero=FALSE, include_land_and_water_area=FALSE, use_lower_resolution_geo_data=TRUE) {
  # section: inspect input
  if (year < 2010 | year > 2019) {
    stop("Year must be in between 2010 and 2019 (inclusive)")
  }
  if (is.na(long) | is.na(lat)) {
    stop("Null lat or long passed to get_acmt_standard_array")
  }

  # section: get ACS context measurements
  acs_info <- get_acs_standard_columns(year=year, codes_of_acs_variables_to_get=codes_of_acs_variables_to_get)
  acs_columns <- acs_info$acs_columns
  acs_proportion_names <- acs_info$acs_proportion_names
  acs_count_names <- acs_info$acs_count_names
  acs_unique_var_cols <- acs_info$acs_unique_var_cols
  acs_count_results <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters, acs_var_names=acs_unique_var_cols, year=year, fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero, include_land_and_water_area=include_land_and_water_area, use_lower_resolution_geo_data=use_lower_resolution_geo_data, variable_name_to_interpolate_by_sum_boolean_mapping=acs_variable_name_to_interpolate_by_sum_boolean_mapping)

  acs_unique_var_cols_contains_missing_variables <- (length(acs_count_results$name) < length(acs_unique_var_cols))

  if(include_land_and_water_area) {  # TODO exclude the two extra variables(ALAND, AWATER) when checking if some ACS variables are missing for this year
    acs_unique_var_cols_contains_missing_variables <- (length(acs_count_results$name) - 2 < length(acs_unique_var_cols))
  }

  if (acs_unique_var_cols_contains_missing_variables) {  # get_acs_standard_columns on only the non-missing variables
    acs_info <- get_acs_standard_columns(year=year, codes_of_acs_variables_to_get=acs_count_results$name)
    acs_columns <- acs_info$acs_columns
    acs_proportion_names <- acs_info$acs_proportion_names
    acs_count_names <- acs_info$acs_count_names
    acs_unique_var_cols <- acs_info$acs_unique_var_cols
  }

  proportion_vals <- vector(length=length(acs_proportion_names))
  for (i in seq_along(proportion_vals)) {

    print(sprintf("Estimate for %s, which is %s will be divided by estimate for %s, which is %s",
                  acs_columns$acs_col[acs_columns$universe_col != ""][i],
                  acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i]))],
                  acs_columns$universe_col[acs_columns$universe_col != ""][i],
                  acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i]))]
    ))
    proportion_vals[i] <-
      acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i]))]/
        acs_count_results$estimate[acs_count_results$name == (as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i]))]
  }
  count_vals <- vector(length=length(acs_count_names))
  for (i in seq_along(count_vals)) {
    count_vals[i] <- acs_count_results$estimate[acs_count_results$name == as.character(acs_columns$acs_col[i])]
  }

  context_measurement_dataframe <- data.frame(names=c(acs_proportion_names, acs_count_names), values=c(proportion_vals, count_vals))
  if (include_land_and_water_area) {  # Add AWATER and ALAND land_area_square_meter, water_area_square_meter
    context_measurement_dataframe[nrow(context_measurement_dataframe) + 1,] <- list("land_area_square_meter", acs_count_results$estimate[acs_count_results$name == "ALAND"])  # append a row
    context_measurement_dataframe[nrow(context_measurement_dataframe) + 1,] <- list("water_area_square_meter", acs_count_results$estimate[acs_count_results$name == "AWATER"])
  }

  if(!is.null(external_data_name_to_info_list)) {
    external_data_list <- load_external_data(external_data_name_to_info_list)

    variable_to_value_dataframe_list <- list()
    for (external_data_name in names(external_data_name_to_info_list)) {
      external_data_weighted_over_point_buffer_dataframe <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters, acs_var_names=NULL, year=NULL, external_data=external_data_list[[external_data_name]], geoid_type = external_data_name_to_info_list[[external_data_name]]$geoid_type, fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero, use_lower_resolution_geo_data=use_lower_resolution_geo_data, variable_name_to_interpolate_by_sum_boolean_mapping=external_data_name_to_info_list[[external_data_name]]$variable_name_to_interpolate_by_sum_boolean_mapping)
      variable_to_value_dataframe <- data.frame(names=external_data_weighted_over_point_buffer_dataframe$name, values=external_data_weighted_over_point_buffer_dataframe$estimate)
      variable_to_value_dataframe_list[[external_data_name]] <- variable_to_value_dataframe
    }
    external_data_measurement_dataframe <- do.call(rbind, variable_to_value_dataframe_list)

    context_measurement_dataframe <- rbind(context_measurement_dataframe, external_data_measurement_dataframe)
  }

  return(context_measurement_dataframe)
}
