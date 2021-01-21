class(point_buffer), class(counties)
class(intersects)
counties$GEOID[c(1,2,3)]
substr(c('asdfad', 'eesag', 'asdfasdf'), 1, 4)
plot(tracts)
plot(water)

plot(tracts_without_water)

tracts[1, ]
tracts_without_water[1, ]

length(tracts)
length(water)
length(tracts_without_water)
length(st_intersection(tracts, water))

view(acs_columns)
count_vals[1:4]
count_results
list(a=1, b=2)$a


acmt_result_list <- get_acmt_standard_array(-122.333, 47.663, 2000, year=2017, return_auxiliary_information=TRUE)



acmt_result_list$radius_meters
acmt_result_list$year
acmt_result_list$acs_info

# count_variable_list <- get_count_variable_for_lat_long(long=acmt_result_list$longitude, lat=acmt_result_list$latitude, radius_meters=acmt_result_list$radius_meters, acs_var_names=acmt_result_list$acs_info$acs_unique_var_cols, year=acmt_result_list$year, return_auxiliary_information=TRUE)

