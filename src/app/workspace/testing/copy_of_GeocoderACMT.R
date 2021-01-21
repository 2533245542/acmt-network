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

acs_columns_2010_url = "http://host.docker.internal:7000/2010ACSColumns.csv"
download.file(url = acs_columns_2010_url, destfile = "ACMT/2010ACSColumns.csv")

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
    water <- st_union(st_as_sf(area_water(state = state, county = county)))
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

# TODO -- vectorize
get_count_variable_for_lat_long <- function(long, lat, radius_meters, acs_var_names, year=year) {
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
  for (i in 1:length(state_county_fips)) {
    tracts_for_state_county <- get_statecounty_tracts(state=block_group_states[i], county=block_group_counties[i], year=year)
    # Census API throws intermittent errors with old years.  Add a retry mechanism to try to track it down
    tries <- 0
    acs_results <- NA
    while (is.na(acs_results) & tries < 10) {
      tries <- tries + 1
      try(
        acs_results <- get_acs("tract", 
                               variables=acs_var_names, 
                               state=block_group_states[i], 
                               county=block_group_counties[i],
                               cache_table = T,
                               census_api_key=CENSUS_API_KEY,
                               geometry = F, keep_geo_vars = T, year=year)
      )
    }
    # HACKHACK 4/18/19 -- skip water merge in order to get results for discussion tomorrow!
    # 8/1/19 -- convert NA to zeros -- is this okay?
    acs_results$estimate[is.na(acs_results$estimate)] <- 0
    acs_results_wide <- dcast(acs_results, GEOID + NAME ~ variable, value.var="estimate" )
    print(nrow(acs_results_wide))
    tracts_for_state_county[,acs_var_names] = acs_results_wide[,acs_var_names]
    block_group_results[[i]]  <- tracts_for_state_county[,acs_var_names]
  }
  if (length(block_group_results) < 1) {
    message("get_count_variable_for_lat_long: No block group data returned from census")
  }
  population <- do.call(rbind, block_group_results)
  population <- st_transform(population, 4326)
  # todo use dplyr to loop over all variables in a prettier way
  #  result <- by(population, population$variable, function(x) { suppressWarnings(st_interpolate_aw(x["estimate"], point_buffer, extensive=T))})
  #    return(result)
  result <- lapply(acs_var_names, function(x) { suppressWarnings(st_interpolate_aw(population[,x], point_buffer, extensive=T)[[2]])})
  return(data.frame(name=acs_var_names, estimate=unlist(result)))  
}

get_acs_standard_columns <- function(year=2017) {
  # To do: cache this
  if (year < 2011) {
    print("Using 2010 ACS columns")
    acs_columns <- read.csv("ACMT/2010ACSColumns.csv")
  } else {
    print("Using Post-2010 ACS columns")
    acs_columns <- read.csv("ACMT/ACSColumns.csv")
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
get_acmt_standard_array <- function(long, lat, radius_meters, year=2017, return_auxiliary_information=FALSE) {
  if (is.na(long) | is.na(lat)) { stop("Null lat or long passed to get_acmt_standard_array")}
  acs_info <- get_acs_standard_columns(year=year)
  acs_columns <- acs_info$acs_columns
  acs_proportion_names <- acs_info$acs_proportion_names
  acs_count_names <- acs_info$acs_count_names
  acs_unique_var_cols <- acs_info$acs_unique_var_cols
  count_results <- get_count_variable_for_lat_long(long, lat, radius_meters, acs_unique_var_cols, year=year)
  #  count_results <- get_count_variable_for_lat_long_accounting_for_water(long, lat, radius_meters, acs_unique_var_cols, year=year)

  proportion_vals <- vector(length=length(acs_proportion_names))
  for (i in 1:length(proportion_vals)) {
    # print(sprintf("Estimate for %s, which is %s will be divided by estimate for %s, which is %s",
    #             acs_columns$acs_col[acs_columns$universe_col != ""][i],
    #             count_results[[as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i])]]$estimate,
    #             acs_columns$universe_col[acs_columns$universe_col != ""][i],
    #             count_results[[as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i])]]$estimate
    #                 ))
    # proportion_vals[i] <- count_results[[as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i])]]$estimate/
    #                     count_results[[as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i])]]$estimate
    
    # 5/17/2019 Using data frame returned from get_count_variable
    print(sprintf("Estimate for %s, which is %s will be divided by estimate for %s, which is %s",
                  acs_columns$acs_col[acs_columns$universe_col != ""][i],
                  count_results$estimate[count_results$name == (as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i]))],
                  acs_columns$universe_col[acs_columns$universe_col != ""][i],
                  count_results$estimate[count_results$name == (as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i]))]
    ))
    proportion_vals[i] <- 
      count_results$estimate[count_results$name == (as.character(acs_columns$acs_col[acs_columns$universe_col != ""][i]))]/
      count_results$estimate[count_results$name == (as.character(acs_columns$universe_col[acs_columns$universe_col != ""][i]))]
  }
  count_vals <- vector(length=length(acs_count_names))
  for (i in 1:length(count_vals)) {
    #    count_vals[i] <- count_results[[as.character(acs_columns$acs_col[i])]]$estimate
    count_vals[i] <- count_results$estimate[count_results$name == as.character(acs_columns$acs_col[i])]
  }
  # WEIPENG: Added an if block. Return a list (with extra information) if flag is true.
  if (return_auxiliary_information) {
    return(list(
      context_measurement=data.frame(names=c(acs_proportion_names, acs_count_names), values=c(proportion_vals, count_vals)),
      longitude=long,
      latitude=lat,
      radius=radius_meters,
      year=year,
      acs_info=acs_info
    ))
  }
  return(data.frame(names=c(acs_proportion_names, acs_count_names), values=c(proportion_vals, count_vals)))
}
