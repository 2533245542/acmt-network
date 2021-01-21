# source("RefreshAPIKey.R")
# source("copy_of_GeocoderACMT.R")


# Test Geocoder
# location <- geocode("1959 NE Pacific Street, Seattle, WA 98195")

# Test ACMT
measures <- get_acmt_standard_array(-122.333, 47.663, 2000, year=2010)  

# Test Both
# location <- geocode("1959 NE Pacific Street, Seattle, WA 98195")
# measures <- get_acmt_standard_array(location$longitude, location$latitude, 2000, year=2010)  
# data.frame(measures$names, round(measures$values, 2))
