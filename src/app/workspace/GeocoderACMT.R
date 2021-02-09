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
  return(list(latitude=latitude, longitude=longitude))
}

if (!dir.exists("ACMT")) {
  dir.create("ACMT")
}

# TEMP -- not able to load this shapefile after downloading from local docker host
#zones_shp_url = "http://localhost:7000/USA_State_Plane_Zones_NAD83.shp"
#zones_shx_url = "http://localhost:7000/USA_State_Plane_Zones_NAD83.shx"
#zones_dbf_url = "http://localhost:7000/USA_State_Plane_Zones_NAD83.dbf"
#zones_prj_url = "http://localhost:7000/USA_State_Plane_Zones_NAD83.prj"
#download.file(url = zones_shp_url, destfile = "ACMT/zones.shp")
#download.file(url = zones_shx_url, destfile = "ACMT/zones.shx")
#download.file(url = zones_dbf_url, destfile = "ACMT/zones.dbf")
#download.file(url = zones_prj_url, destfile = "ACMT/zones.prj")
#state_plane_zones <- sf::st_read(dsn="ACMT", layer="zones")


download.file(url = "http://sandbox.idre.ucla.edu/mapshare/data/usa/other/spcszn83.zip", destfile = "ACMT/spcszn83.zip")
unzip("ACMT/spcszn83.zip", exdir="ACMT")
state_plane_zones <- sf::st_read(dsn="ACMT", layer="spcszn83")


# counties_shp_url = "http://localhost:7000/cb_2017_us_county_500k.shp"
# counties_shx_url = "http://localhost:7000/cb_2017_us_county_500k.shx"
# counties_dbf_url = "http://localhost:7000/cb_2017_us_county_500k.dbf"
# counties_prj_url = "http://localhost:7000/cb_2017_us_county_500k.prj"
# download.file(url = counties_shp_url, destfile = "ACMT/cb_2017_us_county_500k.shp")
# download.file(url = counties_shx_url, destfile = "ACMT/cb_2017_us_county_500k.shx")
# download.file(url = counties_dbf_url, destfile = "ACMT/cb_2017_us_county_500k.dbf")
# download.file(url = counties_prj_url, destfile = "ACMT/cb_2017_us_county_500k.prj")
# counties <- sf::st_read(dsn = "ACMT", layer = "cb_2017_us_county_500k")
# counties <- st_transform(counties, 4326)

download.file(url = "https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_500k.zip", destfile = "ACMT/cb_2017_us_county_500k.zip")
unzip("ACMT/cb_2017_us_county_500k.zip", exdir="ACMT")
counties <- sf::st_read(dsn="ACMT", layer="cb_2017_us_county_500k")
counties <- st_transform(counties, 4326)

acs_columns_2010_url <- "http://host.docker.internal:7000/2010ACSColumns.csv"
download.file(url = acs_columns_2010_url, destfile = "ACMT/2010ACSColumns.csv")

acs_columns_url <- "http://host.docker.internal:7000/ACSColumns.csv"
download.file(url = acs_columns_url, destfile = "ACMT/ACSColumns.csv")

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
get_statecounty_tracts <- function(state, county, year=2017) {
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
    tracts <- st_as_sf(tracts(state = state, county = county, year=year))
    water <- st_union(st_as_sf(area_water(state = state, county = county, year=year)))
    tracts_without_water <- st_difference(tracts, water)
    state_counties[[county]] <- tracts_without_water
  }
  state_list[[state]] <- state_counties
  return(tracts_without_water)
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
      if (error_is_caused_by_missing_variable) {  # remove the missing variable. Note that only one missing variable will be found by per get_acs run.
        front_trimmed_error_message <- sub("Your API call has errors.  The API message returned is error: error: unknown variable '", "", condition$message) # remove the former part
        code_of_the_missing_variable <- sub("E'.", "", front_trimmed_error_message)  # remove the latter part
        acs_var_names <<- acs_var_names[acs_var_names != code_of_the_missing_variable]
        print(paste("Removing missing variable:", code_of_the_missing_variable))
        return(TRUE)
      } else {
        return (FALSE)
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

get_count_variable_for_lat_long <- function(long, lat, radius_meters, acs_var_names=NULL, year=year, external_data=NULL, fill_missing_GEOID_with_zero=FALSE) {  # count_results might not have the variable measures for all GEOIDs in census tracts, in that case, use 0 for the measure; if this is not done, the returned result will be NA
  using_external_data <- FALSE
  var_names <- acs_var_names

  if (is.null(acs_var_names) && is.null(year) && !is.null(external_data)) {  # when no acs_var_names, year, but have external_data
    using_external_data <- TRUE
    var_names <- unique(external_data$variable)
  }

  point_buffer <- get_point_buffer_for_lat_long(long, lat, radius_meters)

  intersects <- st_intersects(point_buffer, counties)
  if (length(intersects) < 1) {
    message("get_count_variable_for_lat_long error: buffer does not overlap US counties")
  }

  state_county_fips <- unique(as.character(counties$GEOID[intersects[[1]]]))
  print(state_county_fips)

  block_group_states <- substr(state_county_fips, 1, 2)
  block_group_counties <- substr(state_county_fips, 3, 5)

  block_group_results <- list()
  for (i in seq_along(state_county_fips)) {
    tracts_for_state_county <- get_statecounty_tracts(state=block_group_states[i], county=block_group_counties[i])

    count_results <- NA

    if (using_external_data) {
      count_results <- external_data
    } else {
      # Census API throws intermittent errors with old years.  Add a retry mechanism to try to track it down
      tries <- 0
      while (length(count_results) == 1 && is.na(count_results) && tries < 10) {  # the first condition ensures the loop breaks without executing is.na (such that no warning is made)
        tries <- tries + 1
        try(
          count_results <- get_acs_results_for_available_variables(
            acs_var_names=var_names,
            state=block_group_states[i],
            county=block_group_counties[i],
            year=year)
        )
      }
      if (length(unique(count_results$variable)) < length(var_names)) {   # if missing variables were pruned, update var_names to let it only include the available variables
        var_names <- var_names[var_names %in% count_results$variable]  # not assigning acs_results$variable directly to var_names because although they are the same, the order of variables is different due to calling get_acs
      }
    }

    count_results$estimate[is.na(count_results$estimate)] <- 0
    count_results_wide <- dcast(count_results, GEOID ~ variable, value.var="estimate" )
    print(nrow(count_results_wide))
    tracts_for_state_county <- left_join(x=tracts_for_state_county, y=count_results_wide, by="GEOID")
    block_group_results[[i]]  <- tracts_for_state_county[,var_names]
  }
  if (length(block_group_results) < 1) {
    message("get_count_variable_for_lat_long: No block group data returned from census")
  }
  population <- do.call(rbind, block_group_results)
  population <- st_transform(population, 4326)
  if (fill_missing_GEOID_with_zero) {
    population[is.na(population)] <- 0
  }
  result <- lapply(var_names, function(x) { suppressWarnings(st_interpolate_aw(population[,x], point_buffer, extensive=T)[[x]])})
  return(data.frame(name=var_names, estimate=unlist(result)))
}


get_acs_standard_columns <- function(year=2017, codes_of_variables_to_get=NA) {
  # To do: cache this
  print("Read ACS columns")
  acs_columns <- read.csv("ACMT/ACSColumns.csv")

  if (!is.na(codes_of_variables_to_get[1])) {  # filter acs_columns by provided variables
    acs_columns <- acs_columns[acs_columns$acs_col %in% codes_of_variables_to_get, ]
  }

  acs_varnames <- acs_columns$acs_col
  print(acs_varnames)
  names(acs_varnames) <- acs_columns$var_name
  acs_proportion_names <- paste(acs_columns$var_name[acs_columns$universe_col != ""], "proportion", sep="_")
  acs_count_names <- paste(acs_columns$var_name, "count", sep="_")
  acs_proportion_pretty_names <- acs_columns$pretty_name_proportion[acs_columns$universe_col != ""]
  acs_count_pretty_names <- acs_columns$pretty_name_count
  all_var_cols <- c(as.character(acs_columns$acs_col), as.character(acs_columns$universe_col))
  unique_var_cols <- unique(all_var_cols)
  unique_var_cols <- unique_var_cols[unique_var_cols != ""]
  return(list(acs_proportion_names=acs_proportion_names, 
              acs_count_names=acs_count_names, 
              acs_unique_var_cols=unique_var_cols, 
              acs_columns=acs_columns, 
              acs_proportion_pretty_name_map=data.frame(acs_proportion_names, acs_proportion_pretty_names), 
              acs_count_pretty_name_map=data.frame(acs_count_names, acs_count_pretty_names)))
}



# TODO: not handling margin of error correctly at all
get_acmt_standard_array <- function(long, lat, radius_meters, year=2017, external_data_name_to_info_list=NULL, fill_missing_GEOID_with_zero=FALSE) {
  # section: inspect input
  if (year < 2010 | year > 2019) {
    stop("Year must be in between 2010 and 2019 (inclusive)")
  }
  if (is.na(long) | is.na(lat)) {
    stop("Null lat or long passed to get_acmt_standard_array")
  }

  # section: get ACS context measurements
  acs_info <- get_acs_standard_columns(year=year)
  acs_columns <- acs_info$acs_columns
  acs_proportion_names <- acs_info$acs_proportion_names
  acs_count_names <- acs_info$acs_count_names
  acs_unique_var_cols <- acs_info$acs_unique_var_cols
  acs_count_results <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters, acs_var_names=acs_unique_var_cols, year=year, fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero)

  acs_unique_var_cols_contains_missing_variables <- (length(acs_count_results$name) < length(acs_unique_var_cols))
  if (acs_unique_var_cols_contains_missing_variables) {  # get_acs_standard_columns on only the non-missing variables
    acs_info <- get_acs_standard_columns(year=year, codes_of_variables_to_get=acs_count_results$name)
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

  if(!is.null(external_data_name_to_info_list)) {
    external_data <- load_external_data(external_data_name_to_info_list)

    external_data_count_results <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters, acs_var_names=NULL, year=NULL, external_data=external_data, fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero)

    external_data_measurement_dataframe <- data.frame(names=external_data_count_results$name, values=external_data_count_results$estimate)

    context_measurement_dataframe <- rbind(context_measurement_dataframe, external_data_measurement_dataframe)

  }

  return(context_measurement_dataframe)
}