setwd("../")
options(stringsAsFactors = FALSE)  # it is default to be FALSE in R4.0 but TRUE in earlier R versions
library(testthat)
source("setup-acmt.R")

test_that("using call 911 data", {
  external_data_name_to_info_list <- list(calls911=external_data_presets_call911)

  result <- get_aggregated_point_measures(latitude = 47.663, longitude = -122.333, radius = 2000, external_data_name_to_info_list=external_data_name_to_info_list)

  expect_equal(filter(result$aggregated_result, variable=="is_call911 1RED 1 Unit")$aggregated_estimate, 617)
  expect_equal(filter(result$aggregated_result, variable=="is_call911 Aid Resp Infectious")$aggregated_estimate, 1)
  expect_equal(filter(result$aggregated_result, variable=="is_call911 Aid Response")$aggregated_estimate, 18132)
  expect_equal(filter(result$aggregated_result, variable=="is_call911 Water Job Minor")$aggregated_estimate, 135)
})


test_that("using call crime boston", {
  external_data_name_to_info_list <- list(crime_boston=external_data_presets_crime_boston)
  result <- get_aggregated_point_measures(latitude = 42.3650142, longitude = -71.0800319, radius = 2000, external_data_name_to_info_list=external_data_name_to_info_list)

  expect_equal(filter(result$aggregated_result, variable=="is_crime_boston ASSAULT - AGGRAVATED")$aggregated_estimate, 201)
  expect_equal(filter(result$aggregated_result, variable=="is_crime_boston AUTO THEFT")$aggregated_estimate, 61)
  #plot_buffer_with_background(buffer=result$created_buffer, longitude=-71.0800319, latitude=42.3650142)
})
