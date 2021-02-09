# if called by test_file, the working directory should be workspace/ (at the same level as RefreshAPIKey.R); otherwise comment this line
setwd("../")

options(stringsAsFactors = FALSE)  # it is default to be FALSE in R4.0 but TRUE in earlier R versions

library(testthat)
source("RefreshAPIKey.R")
source("GeocoderACMT.R")

test_that("load external data", {
  external_data_name_to_info_list <- list(
    mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
               download_file=download_file_mrefi,  # function to download file
               process_file=process_file_mrefi)   # function to process file
  )

  external_data <- load_external_data(external_data_name_to_info_list)

  expect_equal(dim.data.frame(external_data), c(65345, 3))
  expect_equal(pull(external_data[3,2]), "mRFEI")
  expect_equal(pull(external_data[443,3]), 14.28571429)
  expect_equal(pull(external_data[9824,2]), "mRFEI")
  expect_equal(pull(external_data[58882,1]), "48375012000")
  expect_equal(pull(external_data[65345,3]), 18.18181818)

})


test_that("get_acmt_standard_array with external data", {
  external_data_name_to_info_list <- list(
    mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
               download_file=download_file_mrefi,  # function to download file
               process_file=process_file_mrefi)   # function to process file
  )

  measures_for_2013_with_external_data_with_fill_missing <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
  expect_equal(dim(measures_for_2013_with_external_data_with_fill_missing), c(204, 2))
  expect_equal(measures_for_2013_with_external_data_with_fill_missing[204, 1], "mRFEI")
  expect_equal(measures_for_2013_with_external_data_with_fill_missing[204, 2], 159.3582212)

  measures_for_2013_with_external_data_without_fill_missing <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = FALSE)

  expect_equal(dim(measures_for_2013_with_external_data_without_fill_missing), c(204, 2))
  expect_equal(measures_for_2013_with_external_data_without_fill_missing[204, 1], "mRFEI")
  expect_true(is.na(measures_for_2013_with_external_data_without_fill_missing[204, 2]))
})