# if called by test_file, the working directory should be workspace/ (at the same level as RefreshAPIKey.R); otherwise comment this line
setwd("../")
options(stringsAsFactors = FALSE)  # it is default to be FALSE in R4.0 but TRUE in earlier R versions
library(testthat)
source("setup-acmt.R")

test_that("test corretness of get_geoid_for_lat_long", {
  expect_equal(get_geoid_for_lat_long(lat = 47.663, lon = -122.30, geoid_type = "County"), "53033")
  expect_equal(get_geoid_for_lat_long(lat = 47.663, lon = -122.30, geoid_type = "Census Tract"), "53033004301")
  expect_equal(get_geoid_for_lat_long(lat = 47.663, lon = -122.30, geoid_type = "Block Group"), "530330043011")
})

test_that("test corretness of get_geoid_for_lat_long_annonymous", {
  expect_equal(get_geoid_for_lat_long_annonymous(lat = 47.663, lon = -122.30, geoid_type = "County"), "53033")
  expect_equal(get_geoid_for_lat_long_annonymous(lat = 47.663, lon = -122.30, geoid_type = "Census Tract"), "53033004301")
  expect_equal(get_geoid_for_lat_long_annonymous(lat = 47.663, lon = -122.30, geoid_type = "Block Group"), "530330043011")
})


test_that("test buffer plotting with background", {
  latitude <- 47.665505
  longitude <- -122.300000
  point_buffer <- get_point_buffer_for_lat_long(long=longitude, lat=latitude, radius_meters=200)
  m <- plot_buffer_with_background(buffer = point_buffer, longitude = longitude, latitude = latitude)
})