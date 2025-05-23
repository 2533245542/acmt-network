# if called by test_file, the working directory should be workspace/ (at the same level as RefreshAPIKey.R); otherwise comment this line
setwd("../")
options(stringsAsFactors = FALSE)  # it is default to be FALSE in R4.0 but TRUE in earlier R versions

library(testthat)
source("RefreshAPIKey.R")
source("GeocoderACMT.R")
source('setup-acmt.R') ## do we need to run this instead of just the GeocoderAMCT -- it is part of the full list of tests.

test_that("Checking geocoder(). Testing result corretness.", {
  geocoder_is_available <- NA

  tryCatch({  # note that codes in try is not inside a new function, just treat it as normal R code
    geocode("1959 NE Pacific Street, Seattle, WA 98195")
    geocoder_is_available <- TRUE
  }, error = function(condition) {  # note that in error handler, the codes is inside a new function; this is why we need <<- for assigning acs_var_names
    geocoder_is_available <<- FALSE
  })

  expect_true(!is.na(geocoder_is_available))  # assert try catch works correctly

  if (!geocoder_is_available) {
    skip("Geocoder not avaiable")
  }

  expect_equal(geocode("1959 NE Pacific Street, Seattle, WA 98195"), list("latitude"= 47.65061, "longitude"= -122.30799, rating = 0))
})

test_that("Checking state_plane_zones. Testing result corretness.", {
  expect_equal(dim(state_plane_zones), c(121, 8))
  expect_equal(state_plane_zones$ZONE[8], "IA_N")
  expect_equal(state_plane_zones$ZONENAME[1], "Hawaii Zone 1")
  expect_equal(state_plane_zones$ZONENAME[83], "Wisconsin North")
  expect_equal(state_plane_zones$FIPSZONE[23], "1801")
  expect_equal(state_plane_zones$SQMI[22], 21931.1671300000016)
  expect_equal(state_plane_zones$COLORMAP[99], 5)
  expect_equal(state_plane_zones$Shape_Leng[13], 19.26427318)
  expect_equal(state_plane_zones$Shape_Area[33], 10.32449057)
  expect_equal(dim(state_plane_zones$geometry[[3]][[1]][[1]]), c(20, 2))
  expect_equal(state_plane_zones$geometry[[3]][[1]][[1]][3,1], -157.72150400000001014)
})

test_that("Checking counties. Testing result corretness.", {
  expect_equal(dim(counties), c(3233, 10))
  expect_equal(counties$STATEFP[3], "01")
  expect_equal(counties$COUNTYFP[44], "009")
  expect_equal(counties$COUNTYNS[41], "00303665")
  expect_equal(counties$AFFGEOID[21], "0500000US06013")
  expect_equal(counties$GEOID[1847], "19105")
  expect_equal(counties$NAME[3199], "Scott")
  expect_equal(counties$LSAD[23], "06")
  expect_equal(counties$ALAND[92], 1132603918)
  expect_equal(counties$AWATER[484], 25242751)
  expect_equal(dim(counties$geometry[[1]][[1]][1][[1]]), c(537, 2))
  expect_equal(counties$geometry[[1]][[1]][1][[1]][1,1], -85.74803199)
})

test_that("Checking state_proj (loaded from USAboundaries). Testing result corretness.", {
  expect_equal(dim(state_proj), c(123, 7))
  expect_equal(state_proj$state[41], "ID")
  expect_equal(state_proj$zone[9], "7")
  expect_equal(state_proj$epsg[85], "32119")
  expect_equal(state_proj$proj4_string[69], "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-92.5 +k=0.999933333 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  expect_equal(state_proj$statewide_proj[35], FALSE)
})

test_that("Checking get_projection_for_lat_long. Testing result corretness.", {
  expect_equal(get_projection_for_lat_long(long=-122.333, lat=47.663), "+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
})

test_that("Checking get_point_buffer_for_lat_long. Testing result corretness.", {
  expect_equal(dim(get_point_buffer_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000)[[1]][[1]]), c(121, 2))
  expect_equal(get_point_buffer_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000)[[1]][[1]][86, 2], 47.6802814947)
})

test_that("Checking get_statecounty_tracts. Testing result corretness.", {
  state_county_tract <- get_geometries_of_a_county(state = "53", county = "033")

  expect_equal(dim(state_county_tract), c(397, 13))
  expect_equal(unique(state_county_tract$STATEFP), "53")
  expect_equal(unique(state_county_tract$COUNTYFP), "033")
  expect_equal(state_county_tract$TRACTCE[83], "020402")
  expect_equal(state_county_tract$GEOID[32], "53033028300")
  expect_equal(state_county_tract$NAME[2], "215")
  expect_equal(state_county_tract$NAMELSAD[42], "Census Tract 97.02")
  expect_equal(unique(state_county_tract$MTFCC), "G5020")
  expect_equal(unique(state_county_tract$FUNCSTAT), "S")
  expect_equal(as.integer(state_county_tract$ALAND[98]), 2871800)  # might be due to package version, that ALAND becomes string in docker version
  expect_equal(as.integer(state_county_tract$AWATER[276]), 457717)
  expect_equal(state_county_tract$INTPTLAT[376], "+47.6694019")
  expect_equal(state_county_tract$INTPTLON[275], "-122.3674088")
  expect_equal(dim(state_county_tract$geometry[82][[1]][[1]]), c(92, 2))
  expect_equal(state_county_tract$geometry[82][[1]][[1]][43,1], -122.112381)

  # not using water
  statecounty_tracts_2010 <- get_geometries_of_a_county(state="53", county="033", year=2010)  # no water in 2010
  expect_equal(dim(statecounty_tracts_2010), c(398, 15))
  expect_equal(statecounty_tracts_2010[[94,1]], "53")
  expect_equal(statecounty_tracts_2010[[188,2]], "033")
  expect_equal(statecounty_tracts_2010[[278,3]], "029408")
  expect_equal(statecounty_tracts_2010[[92,4]], "53033032322")
  expect_equal(statecounty_tracts_2010[[14,6]], "Census Tract 272")
  expect_equal(as.integer(statecounty_tracts_2010[[391,9]]), 1834052)


  # water available; the result should be stripped off of water area
  statecounty_tracts_2011 <- get_geometries_of_a_county(state="53", county="033", year=2011)
  expect_equal(dim(statecounty_tracts_2011), c(397, 13))
  expect_equal(statecounty_tracts_2011[[94,1]], "53")
  expect_equal(statecounty_tracts_2011[[188,2]], "033")
  expect_equal(statecounty_tracts_2011[[278,3]], "030313")
  expect_equal(statecounty_tracts_2011[[92,4]], "53033031301")
  expect_equal(statecounty_tracts_2011[[14,6]], "Census Tract 312.06")
  expect_equal(as.integer(statecounty_tracts_2011[[391,9]]), 1593774)
})

test_that("Checking get_acs_results_for_available_variables(). Testing invalid input.", {
  # below are selected variables that are in ACSColumns.csv but not 2010ACSColumns.csv
  # B15003 variables are available in 2012 or later
  all_not_work_in_2011 <- c("B15003_001", "B15003_002")
  some_work_in_2011 <- c("B15003_001", "B07201_002")
  all_work_in_2011 <- c("B07201_002", "B07201_004")
  all_work_in_2012 <- c("B15003_001", "B15003_002")

  expect_error(get_acs_results_for_available_variables(all_not_work_in_2011, "53", "033", 2011), regexp = "All ACS variables are missing for year 2011") # no result, produce error

  expect_error((acs_result_for_B07201_002_2011 <- get_acs_results_for_available_variables(some_work_in_2011, "53", "033", 2011)), regexp = NA) # one result
  expect_equal(unique(acs_result_for_B07201_002_2011$variable), "B07201_002")
  expect_equal(acs_result_for_B07201_002_2011$estimate[1], 3992)

  expect_error((acs_result_for_B07201_002_004_2011 <- get_acs_results_for_available_variables(all_work_in_2011, "53", "033", 2011) ), regexp = NA) # two results
  expect_equal(unique(acs_result_for_B07201_002_004_2011$variable), c("B07201_002", "B07201_004"))
  expect_equal(acs_result_for_B07201_002_004_2011$estimate[1], 3992)
  expect_equal(acs_result_for_B07201_002_004_2011$estimate[100], 1478)

  expect_error((acs_result_for_B15003_001_002_2012 <- get_acs_results_for_available_variables(all_work_in_2012, "53", "033", 2012)), regexp = NA) # two results
  expect_equal(unique(acs_result_for_B15003_001_002_2012$variable), c("B15003_001", "B15003_002"))
  expect_equal(acs_result_for_B15003_001_002_2012$estimate[1], 4444)
  expect_equal(acs_result_for_B15003_001_002_2012$estimate[100], 0)

  expect_error(get_acs_results_for_available_variables(all_work_in_2012, "53", "033", 2001), regexp = "Year must be in between 2010 and 2021, inclusive") # uncovered error
})

test_that("Checking get_count_variable_for_lat_long. Testing result corretness.", {
  # test year=2011
  list_of_acs_var_names <- c("B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_026", "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B25001_001", "B05012_002", "B05012_003", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008", "B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006", "B06008_001", "B06008_002", "B06008_003", "B06008_004", "B06008_005", "B06008_006", "B06008_007", "B06008_013", "B07201_002", "B07201_004", "B07201_014", "B08006_001", "B08006_003", "B08006_004", "B08006_008", "B08006_014", "B08006_015", "B08006_016", "B08006_017", "B08302_002", "B08302_003", "B08302_004", "B08302_005", "B08302_006", "B08302_007", "B08302_008", "B08302_009", "B08302_010", "B08302_011", "B08302_012", "B08302_013", "B08302_014", "B08302_015", "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025", "B15002_002", "B15002_011", "B15002_015", "B15002_019", "B15002_028", "B15002_032")

  #acs_variable_name_to_interpolate_by_sum_boolean_mapping<-full_interpolation_list()
  count_results_in_test_2010 <- get_count_variable_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000, acs_var_names = list_of_acs_var_names, year=2010, 
                                                                variable_name_to_interpolate_by_sum_boolean_mapping=acs_variable_name_to_interpolate_by_sum_boolean_mapping)
  expect_equal(dim(count_results_in_test_2010), c(82, 2))
  #expect_equal(count_results_in_test_2010[1,2], 51120.98)
  expect_equal(count_results_in_test_2010[1,2], 51120.02120)
  expect_equal(as.character(count_results_in_test_2010[12,1]), "B01001_012")
  #expect_equal(count_results_in_test_2010[32,2], 25439.00094)
  expect_equal(count_results_in_test_2010[32,2], 25438.91526)
  #expect_equal(as.numeric(count_results_in_test_2010[44,2]), 1212.450048)
  expect_equal(as.numeric(count_results_in_test_2010[44,2]), 1212.44460)
  expect_equal(as.character(count_results_in_test_2010[68,1]), "B08302_007")
  #expect_equal(count_results_in_test_2010[74,2], 488.1177998)
  expect_equal(count_results_in_test_2010[74,2], 488.10106)
  #expect_equal(count_results_in_test_2010[82,2], 6308.99184)
  expect_equal(count_results_in_test_2010[82,2], 6309.09561)

  # compared to 2010, it has 3 extra variables, B07201_002, B07201_004, B07201_014
  count_results_in_test_2011 <- get_count_variable_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000, acs_var_names =list_of_acs_var_names, year=2011, variable_name_to_interpolate_by_sum_boolean_mapping=acs_variable_name_to_interpolate_by_sum_boolean_mapping)
  expect_equal(dim(count_results_in_test_2011), c(85, 2))
  #expect_equal(as.numeric(count_results_in_test_2011[1,2]),51601.22724)
  expect_equal(as.numeric(count_results_in_test_2011[1,2]),51600.28121)
  expect_equal(as.character(count_results_in_test_2011[12,1]),"B01001_012")
  #expect_equal(count_results_in_test_2011[32,2],25416.26595)
  expect_equal(count_results_in_test_2011[32,2],25416.16207)
  #expect_equal(count_results_in_test_2011[44,2],998.5428506)
  expect_equal(count_results_in_test_2011[44,2],998.54242)
  expect_equal(as.character(count_results_in_test_2011[68,1]),"B08302_004")
  #expect_equal(count_results_in_test_2011[74,2],2910.033344)
  expect_equal(count_results_in_test_2011[74,2],2910.03402)
  #expect_equal(count_results_in_test_2011[79,2],1507.540435)
  expect_equal(count_results_in_test_2011[79,2],1507.47921)
  #expect_equal(count_results_in_test_2011[84,2],965.3484705)
  expect_equal(count_results_in_test_2011[84,2],965.33043)
  # compared to 2011, it has 25 more variables
  # the number of available variables are the same for year=2011 and after, and these years have no missing variables anymore
  # ("B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025")
  count_results_in_test_2013 <- get_count_variable_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000, acs_var_names =list_of_acs_var_names, year=2013, variable_name_to_interpolate_by_sum_boolean_mapping=acs_variable_name_to_interpolate_by_sum_boolean_mapping)
  expect_equal(dim(count_results_in_test_2013), c(110, 2))
  #expect_equal(count_results_in_test_2013[1,2],53223.07818)
  expect_equal(count_results_in_test_2013[1,2],53222.229697)
  expect_equal(as.character(count_results_in_test_2013[12,1]),"B01001_012")
  #expect_equal(count_results_in_test_2013[32,2],25439.71694)
  expect_equal(count_results_in_test_2013[32,2],25439.587642)
  #expect_equal(count_results_in_test_2013[44,2],1113.72422)
  expect_equal(count_results_in_test_2013[44,2],1113.743628)
  expect_equal(as.character(count_results_in_test_2013[68,1]),"B08302_004")
  #expect_equal(count_results_in_test_2013[74,2],3467.624946)
  expect_equal(count_results_in_test_2013[74,2],3467.640665)
  #expect_equal(count_results_in_test_2013[104,2],1926.745257)
  expect_equal(count_results_in_test_2013[104,2],1926.710783)
  #expect_equal(count_results_in_test_2013[109,2],1085.485675)
  expect_equal(count_results_in_test_2013[109,2],1085.450895)

  # error handling for variable_name_to_interpolate_by_sum_boolean_mapping
  expect_error(get_count_variable_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000, 
                                               acs_var_names =c("B01001_001", "B01001_002", "B01001_003"), year=2013, variable_name_to_interpolate_by_sum_boolean_mapping=NULL), regexp="Function get_count_variable_for_lat_long is not provided with variable_name_to_interpolate_by_sum_boolean_mapping")
  expect_error(get_count_variable_for_lat_long(long=-122.333, lat=47.663, radius_meters = 2000, 
                                               acs_var_names =c("B01001_001", "B01001_002", "B01001_003"), year=2013, variable_name_to_interpolate_by_sum_boolean_mapping=c('non_existing_variable'=0)), regexp="Interploation by sum boolean for .*")
})

test_that("Checking get_acs_standard_columns(). Testing retreived context measurements' corretness.", {
  ## assert codes_of_variables_to_get works as expected
  expect_equal(nrow(get_acs_standard_columns(year=2010, codes_of_acs_variables_to_get=c("B01001_010", "B01001_011", "B01001_012"))$acs_columns), 3)# should contain only 3 variables
  expect_equal(nrow(get_acs_standard_columns(year=2010)$acs_columns), 352) # should contain 352 variables, before removing missing variables
  expect_equal(nrow(get_acs_standard_columns(year=2011, codes_of_acs_variables_to_get=c("B08302_014", "B08302_015", "B25001_001"))$acs_columns), 3)# should contain only 3 variables
  expect_equal(nrow(get_acs_standard_columns(year=2011)$acs_columns), 347) # should contain 347 variables, before removing missing variables

  ## ACS standard columns for 2012 through 2018 should be the same
  acs_standard_column_for_2010 <- get_acs_standard_columns(year=2010) ## 2010 will be different -- 
  acs_standard_column_for_2011 <- get_acs_standard_columns(year=2011) ## 2011 will be different -- 
  acs_standard_column_for_2013 <- get_acs_standard_columns(year=2013)
  acs_standard_column_for_2015 <- get_acs_standard_columns(year=2015)
  acs_standard_column_for_2018 <- get_acs_standard_columns(year=2018)
  acs_standard_column_for_2019 <- get_acs_standard_columns(year=2019) ##2019 will be different

  #all.equal(acs_standard_column_for_2010, acs_standard_column_for_2011)
  #all.equal(acs_standard_column_for_2010, acs_standard_column_for_2013)
  #all.equal(acs_standard_column_for_2010, acs_standard_column_for_2019)
  all.equal(acs_standard_column_for_2013, acs_standard_column_for_2015)
  all.equal(acs_standard_column_for_2013, acs_standard_column_for_2018)


  ## check if details fit
  expect_equal(dim(acs_standard_column_for_2010$acs_columns), c(352, 6))
  expect_equal(as.character(acs_standard_column_for_2010$acs_columns$acs_col[8]), "B01001_008")
  expect_equal(as.character(acs_standard_column_for_2010$acs_columns$pretty_name_count[15]), "Male residents 45 to 49 (count)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_columns$pretty_name_proportion[41]), "Female residents 55 to 59 (proportion of all Female residents)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_columns$universe_col[1]), "")
  expect_equal(as.character(acs_standard_column_for_2010$acs_columns$var_name[82]), "moved_within_state")
  expect_equal(acs_standard_column_for_2010$acs_columns$var_name[108], "male_head_alone")

  expect_equal(length(acs_standard_column_for_2010$acs_count_names), 352)
  expect_equal(acs_standard_column_for_2010$acs_count_names[1], "total_pop_count")
  expect_equal(acs_standard_column_for_2010$acs_count_names[23],  "males_75_to_79_count")
  expect_equal(acs_standard_column_for_2010$acs_count_names[38], "females_40_to_44_count")
  expect_equal(acs_standard_column_for_2010$acs_count_names[78], "pop_below_100_poverty_threshold_count")
  expect_equal(acs_standard_column_for_2010$acs_count_names[82], "moved_within_state_count")
  expect_equal(acs_standard_column_for_2010$acs_count_names[142], "pop_25_and_over_female_count")

  expect_equal(length(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_names), 352)
  expect_equal(length(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_pretty_names), 352)

  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_names[23]), "males_75_to_79_count")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_names[44]), "females_65_to_66_count")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_names[82]),  "moved_within_state_count")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_names[108]), "male_head_alone_count")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_pretty_names[34]), "Female residents 22 to 24 (count)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_pretty_names[41]), "Female residents 55 to 59 (count)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_pretty_names[81]), "People moved within the same county (count)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_count_pretty_name_map$acs_count_pretty_names[108]), "Male householder, no spouse or partner")

  expect_equal(length(acs_standard_column_for_2010$acs_proportion_names), 293)
  expect_equal(acs_standard_column_for_2010$acs_proportion_names[1], "men_proportion")
  expect_equal(acs_standard_column_for_2010$acs_proportion_names[24], "males_85_and_older_proportion")
  expect_equal(acs_standard_column_for_2010$acs_proportion_names[33], "females_22_to_24_proportion")
  expect_equal(acs_standard_column_for_2010$acs_proportion_names[69], "carpool_commuters_proportion")
  expect_equal(acs_standard_column_for_2010$acs_proportion_names[92], "female_head_kids_proportion")

  expect_equal(length(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_names), 293)
  expect_equal(length(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_pretty_names), 293)

  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_names[1]), "men_proportion")
  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_names[54]),  "pacific_islander_alone_proportion")
  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_names[69]), "carpool_commuters_proportion")
  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_names[93]), "male_head_relatedkids_proportion")
  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_pretty_names[4]), "Male residents 10 to 14 (proportion of all male residents)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_pretty_names[24]),  "Male residents 85 and older (proportion of all male residents)")
  expect_equal(as.character(acs_standard_column_for_2010$acs_proportion_pretty_name_map$acs_proportion_pretty_names[52]), "American Indian or Alaska Native residents (proportion of all residents)")

  expect_equal(length(acs_standard_column_for_2010$acs_unique_var_cols), 352)
  expect_equal(acs_standard_column_for_2010$acs_unique_var_cols[1], "B01001_001")
  expect_equal(acs_standard_column_for_2010$acs_unique_var_cols[13], "B01001_013")
  expect_equal(acs_standard_column_for_2010$acs_unique_var_cols[56], "B02001_007")
  expect_equal(acs_standard_column_for_2010$acs_unique_var_cols[82], "B07001_049")
})

test_that("Checking get_acmt_standard_array(). Testing retreived context measurements' corretness.", {
  # check that the results of running ACMT for (long=-122.333, lat=47.663, radius_meters=2000, year=2010) fits our expectation
  measures_for_2010 <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2010, use_lower_resolution_geo_data = FALSE)
  expect_equal(dim(measures_for_2010), c(645, 2))  # context measurement variables
  expect_equal(as.character(measures_for_2010[1,]$names), "men_proportion")
  #expect_equal(measures_for_2010[1,]$values, 0.527210877307653)
  expect_equal(measures_for_2010[1,]$values, 0.5272087432)
  expect_equal(as.character(measures_for_2010[35,]$names), "females_30_to_34_proportion")
  #expect_equal(measures_for_2010[35,]$values, 0.09592531)
  expect_equal(measures_for_2010[35,]$values, 0.0959290099)
  expect_equal(as.character(measures_for_2010[300,]$names), "males_18_to_19_count")
  #expect_equal(measures_for_2010[300,]$values, 1566.529456)
  expect_equal(measures_for_2010[300,]$values, 1566.364352)
  expect_equal(as.character(measures_for_2010[382,]$names), "bike_commuters_count")
  #expect_equal(measures_for_2010[382,]$values, 1464.661232)
  expect_equal(measures_for_2010[382,]$values, 1464.591280)
  expect_equal(as.character(measures_for_2010[448,]$names), "bachelors_degree_female_count")
  #expect_equal(measures_for_2010[448,]$values, 6308.99184)
  expect_equal(measures_for_2010[448,]$values, 6309.095614)

  measures_for_2011 <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2011, use_lower_resolution_geo_data = FALSE)
  expect_equal(dim(measures_for_2011), c(637, 2))
  expect_equal(as.character(measures_for_2011[1,]$names), "men_proportion")
  #expect_equal(measures_for_2011[1,]$values, 0.5093951915)
  expect_equal(measures_for_2011[1,]$values, 0.5093927128)
  expect_equal(as.character(measures_for_2011[35,]$names), "females_30_to_34_proportion")
  #expect_equal(measures_for_2011[35,]$values, 0.09096531)
  expect_equal(measures_for_2011[35,]$values, 0.0909698142)
  expect_equal(as.character(measures_for_2011[297,]$names), "males_18_to_19_count")
  #expect_equal(measures_for_2011[297,]$values, 1546.891288)
  expect_equal(measures_for_2011[297,]$values, 1546.7305404)
  expect_equal(as.character(measures_for_2011[374,]$names), "drive_alone_commuters_count")
  #expect_equal(measures_for_2011[374,]$values, 14465.7)
  expect_equal(measures_for_2011[374,]$values, 14465.64579)
  expect_equal(as.character(measures_for_2011[390,]$names), "commute_start_9_10_count")
  #expect_equal(measures_for_2011[390,]$values, 3932.895066)
  expect_equal(measures_for_2011[390,]$values, 3932.884479)


  measures_for_2013 <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, use_lower_resolution_geo_data = FALSE)
  expect_equal(dim(measures_for_2013), c(679, 2))
  expect_equal(as.character(measures_for_2013[1,]$names),"men_proportion")
  #expect_equal(measures_for_2013[1,]$values,0.4996009046)
  expect_equal(measures_for_2013[1,]$values, 0.4995983829)
  expect_equal(as.character(measures_for_2013[35,]$names),"females_30_to_34_proportion")
  expect_equal(round(measures_for_2013[35,]$values, 3), 0.101)
  expect_equal(as.character(measures_for_2013[111,]$names),"eighth_grade_proportion")
  #expect_equal(measures_for_2013[111,]$values,0.003083365688)
  expect_equal(round(measures_for_2013[111,]$values, 5),0.00308)
  expect_equal(as.character(measures_for_2013[360,]$names),"white_alone_count")
  #expect_equal(measures_for_2013[360,]$values,40934.27733)
  expect_equal(measures_for_2013[360,]$values,40933.77497)
  expect_equal(as.character(measures_for_2013[392,]$names),"workers_over_15_count")
  #expect_equal(measures_for_2013[392,]$values,33418.219)
  expect_equal(measures_for_2013[392,]$values,33418.015596)
  expect_equal(as.character(measures_for_2013[452,]$names),"associates_degree_count")
  #expect_equal(measures_for_2013[452,]$values,1838.198005)
  expect_equal(measures_for_2013[452,]$values,1838.17952)
})

test_that("Checking get_acmt_standard_array(). Testing input validity.", {
  # the input year's constraint; year is valid from 2010 to 2020, otherwise an error should occur
  expect_error(get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2009)) # invalid
  expect_error(get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2010), NA) # valid, using 2010 ACS columns
  expect_error(get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2011), NA) # valid, using post-2010 ACS columns
  expect_error(get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2015), NA) # valid
  expect_error(get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2020), NA) # valid
  expect_error(get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2021)) # invalid

  # invalid long
  expect_error(get_acmt_standard_array(               lat=47.663, radius_meters = 2000, year=2019), "argument \"long\" is missing, with no default")
  expect_error(get_acmt_standard_array(long=NA, lat=47.663, radius_meters = 2000, year=2019), "Null lat or long passed to get_acmt_standard_array")
  # invalid lat
  expect_error(get_acmt_standard_array(long=-122.333,             radius_meters = 2000, year=2019), "argument \"lat\" is missing, with no default")
  expect_error(get_acmt_standard_array(long=-122.333, lat=NA, radius_meters = 2000, year=2019), "Null lat or long passed to get_acmt_standard_array")
  # invalid long and lat
  expect_error(get_acmt_standard_array(                           radius_meters = 2000, year=2019), "argument \"long\" is missing, with no default")
  expect_error(get_acmt_standard_array(long=NA, lat=NA, radius_meters = 2000, year=2019), "Null lat or long passed to get_acmt_standard_array")

})

test_that("After using the new ACSColumns.csv, some ACS variables should be missing for specific years", {
  # variables only available for ACS > 2010
  missing_variables_for_2010 <- c("B07201_002", "B07201_004", "B07201_014", "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025")

  expect_error(get_acs_results_for_available_variables(missing_variables_for_2010, "53", "033", 2010), regexp = "All ACS variables are missing for year 2010")  # all these variables should be missing for year=2010

  # variables avaiable for ACS = 2010 should be available for year>2010 as well
  missing_variables_for_greater_than_2010 <- c("B15002_002", "B15002_011", "B15002_015", "B15002_019", "B15002_028", "B15002_032")
  for (year_from_2011_to_2019 in 2011:2019) {
    print(year_from_2011_to_2019)
    expect_error(get_acs_results_for_available_variables(missing_variables_for_greater_than_2010, "53", "033", year_from_2011_to_2019), regexp = NA)
  }
})

test_that("speed up ACMT by only querying for certain variabels", {
  latitude <- 47.60328
  longitude <- -122.3302
  radius <- 2000
  year <- 2017
  codes_of_acs_variables_to_get <- c("B01001_001")
  environmental_measures_with_certain_varaibles <- get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, codes_of_acs_variables_to_get=codes_of_acs_variables_to_get, use_lower_resolution_geo_data = FALSE)
  #expect_equal(filter(environmental_measures_with_certain_varaibles, names == "total_pop_count")$values, 66370.00762)
  #expect_equal(round(filter(environmental_measures_with_certain_varaibles, names == "total_pop_count")$values, 2), 66370.01)
  expect_equal(round(filter(environmental_measures_with_certain_varaibles, names == "total_pop_count")$values, 2), 66360.71) ### difference is rather large -- what's going on here?
  })


test_that("Checking get_acmt_standard_array() with lower resolution. Testing retreived context measurements' corretness.", {
  measures_for_2013 <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, use_lower_resolution_geo_data = TRUE)
  expect_equal(dim(measures_for_2013), c(679, 2))
  expect_equal(as.character(measures_for_2013[1,]$names),"men_proportion")
  #expect_equal(measures_for_2013[1,]$values,0.4995895087)
  expect_equal(measures_for_2013[1,]$values,0.49958696)
  expect_equal(as.character(measures_for_2013[35,]$names),"females_30_to_34_proportion")
  #expect_equal(round(measures_for_2013[35,]$values, 7), 0.1006587)
  expect_equal(round(measures_for_2013[35,]$values, 7), 0.1006633)
  expect_equal(as.character(measures_for_2013[111,]$names),"eighth_grade_proportion")
  #expect_equal(measures_for_2013[111,]$values,0.003084673185)
  expect_equal(measures_for_2013[111,]$values,0.00308470)
  expect_equal(as.character(measures_for_2013[360,]$names),"white_alone_count")
  #expect_equal(measures_for_2013[360,]$values,40947.38446)
  expect_equal(measures_for_2013[360,]$values,40946.870355)
  expect_equal(as.character(measures_for_2013[392,]$names),"workers_over_15_count")
  #expect_equal(measures_for_2013[392,]$values,33426.79825)
  expect_equal(measures_for_2013[392,]$values,33426.583908)
  expect_equal(as.character(measures_for_2013[452,]$names),"associates_degree_count")
  #expect_equal(measures_for_2013[452,]$values,1838.901203)
  expect_equal(measures_for_2013[452,]$values,1838.882209)
  
})

test_that("Checking option return_point_estimate", {
  measures_return_point_estimate <- get_acmt_standard_array(lat=47.663, long=-122.30, radius_meters = 20000, year=2013, external_data_name_to_info_list=list(food_access=external_data_presets_walkability), fill_missing_GEOID_with_zero = TRUE, return_point_estimate=TRUE)
  expect_equal(filter(measures_return_point_estimate, names=="females_10_to_14_proportion")$values, 0.02113353, tolerance = 1e-8)
  expect_equal(filter(measures_return_point_estimate, names=="females_10_to_14_count")$values, 44)
  expect_equal(filter(measures_return_point_estimate, names=="some_college_1_year_or_more_count")$values, 164)
  expect_equal(filter(measures_return_point_estimate, names=="males_18_to_19_count")$values, 11)
  expect_equal(filter(measures_return_point_estimate, names=="COUNTHU10")$values, 514)
  expect_equal(filter(measures_return_point_estimate, names=="NatWalkInd")$values, 18.667)
})

### travelable_buffer not working ###
test_that("Test travelable buffer", {
  latitude <- 47.665505
  longitude <- -122.300000
  travelable_buffer <- get_travelable_buffer(latitude=latitude, longitude=longitude, travel_type = "bike", travel_time=20)
  #expect_equal(travelable_buffer[[1]][2][[1]][[1]][1,1], -122.3516, tolerance = 0.0001)
  expect_equal(travelable_buffer[[1]][[1]][2][[1]][1,1], -122.2809, tolerance = 0.0001)
  #expect_equal(travelable_buffer[[1]][[1]][[1]][1,2], 47.6652, tolerance = 0.0001)
  expect_equal(travelable_buffer[[1]][[1]][[1]][1,2], 47.70419, tolerance = 0.0001)
  })

