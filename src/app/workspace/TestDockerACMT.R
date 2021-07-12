source("RefreshAPIKey.R")
source("GeocoderACMT.R")

# Test Geocoder
location <- geocode("1959 NE Pacific Street, Seattle, WA 98195")

# Test ACMT
measures <- get_acmt_standard_array(long=-122.333, lat=47.663, 2000, year=2010)

# Test Both
location <- geocode("1959 NE Pacific Street, Seattle, WA 98195")
measures <- get_acmt_standard_array(long=location$longitude, lat=location$latitude, 2000, year=2010)
data.frame(measures$names, round(measures$values, 2))

# Use external data source (the below uses mRFEI)
external_data_name_to_info_list <- list(
  mrfei=external_data_presets_mrfei
)

measures_for_2013_with_external_data_with_fill_missing <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)

