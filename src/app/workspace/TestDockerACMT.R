source("setup-acmt.R")

# Test Geocoder
location <- geocode("1959 NE Pacfic Street, Seattle, WA 98195")

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

external_data_name_to_info_list <- list(
  park=external_data_presets_park
)

park_shp <- shp_preprocess(shp_directory = "external_data/ParkServe_shp/Shapefiles_08062021/ParkServe_shp_DataShare_08062021/ParkServe_Parks.shp")
proportion <- get_proportion_in_shapefile(long=-122.333, lat=47.663, radius_meters = 2000, shp_processed = park_shp)
dist_min <- get_distance_to_shapefile(long=-122.333, lat=47.663,radius_meters = 50, shp_processed = park_shp)
map <- check_geocode(long = -122.308, lat = 47.65061, address = "1959 NE Pacfic Street, Seattle, WA 98195")
map <- check_geocode_for_address(address = "1959 NE Pacfic Street, Seattle, WA 98195")


school_addresses <- c("520 NE Ravenna Blvd, Seattle, WA 98115", "4649 Sunnyside Ave N #242, Seattle, WA 98103","7400 25th Ave NE, Seattle, WA 98115",
                     "3311 NE 60th St, Seattle, WA 98115", "4057 5th Ave NE, Seattle, WA 98105")
hospital_addresses <- c("1629 N 45th St, Seattle, WA 98103", "345 106th Ave NE, Bellevue, WA 98004","1800 NW Myhre Rd, Silverdale, WA 98383",
                       "1550 N 115th St, Seattle, WA 98133", "2211 NE 139th St, Vancouver, WA 98686")
restaurant_addresses <- c("7545 Lake City Way NE, Seattle, WA 98115", "1303 NE Boat St, Seattle, WA 98105","308 N 125th St, Seattle, WA 98133",
                          "8917 Roosevelt Way NE, Seattle, WA 98115", "11325 NE 124th St, Kifrkland, WA 98034")
business_addresses <-c("733 7th Ave #203, Kirkland, WA 98033","4554 Thackeray Pl NE, Seattle, WA 98105","2720 3rd Ave #802, Seattle, WA 98121",
                       "PG HANSEN BUILDING, 2408 N 45th St, Seattle, WA 98103","425 Urban Plz Suite#223, Kirkland, WA 98033")
map <- check_geocode_for_address(address = restaurant_addresses[5])
print(map)
