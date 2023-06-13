# if called by test_file, the working directory should be workspace/ (at the same level as RefreshAPIKey.R); otherwise comment this line
#setwd("../")
setwd('~/workspace')
source("setup-acmt.R")
options(stringsAsFactors = FALSE)  # it is default to be FALSE in R4.0 but TRUE in earlier R versions

library(testthat)
source("RefreshAPIKey.R")
source("GeocoderACMT.R")

test_that("load external data mrfei", {
  external_data_name_to_info_list <- list(
    mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
               download_file=download_file_mrefi,  # function to download file
               process_file=process_file_mrefi,   # function to process file
               variable_name_to_interpolate_by_sum_boolean_mapping=mrfei_variable_name_to_interpolate_by_sum_boolean_mapping)
  )

  external_data <- load_external_data(external_data_name_to_info_list)[['mrfei']]

  expect_equal(dim.data.frame(external_data), c(65345, 3))
  expect_equal(pull(external_data[3,2]), "mRFEI")
  expect_equal(pull(external_data[443,3]), 14.28571429)
  expect_equal(pull(external_data[9824,2]), "mRFEI")
  expect_equal(pull(external_data[58882,1]), "48375012000")
  expect_equal(pull(external_data[65345,3]), 18.18181818)
})

test_that("get_acmt_standard_array with external data mrfei", {
  external_data_name_to_info_list <- list(
    mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
               download_file=download_file_mrefi,  # function to download file
               process_file=process_file_mrefi,   # function to process file
               geoid_type="Census Tract",
               variable_name_to_interpolate_by_sum_boolean_mapping=mrfei_variable_name_to_interpolate_by_sum_boolean_mapping
    )
  )

  measures_for_2013_with_external_data_with_fill_missing <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE, use_lower_resolution_geo_data = FALSE)
  expect_equal(dim(measures_for_2013_with_external_data_with_fill_missing), c(680, 2))
  expect_equal(measures_for_2013_with_external_data_with_fill_missing[680, 1], "mRFEI")
  expect_equal(measures_for_2013_with_external_data_with_fill_missing[680, 2], 15.115597)

  measures_for_2013_with_external_data_without_fill_missing <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = FALSE, use_lower_resolution_geo_data = FALSE)

  expect_equal(dim(measures_for_2013_with_external_data_without_fill_missing), c(680, 2))
  expect_equal(measures_for_2013_with_external_data_without_fill_missing[680, 1], "mRFEI")
  expect_true(is.na(measures_for_2013_with_external_data_without_fill_missing[680, 2]))
})

test_that("load external data walkability", {
  external_data_name_to_info_list <- list(


  walkability=list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                   download_file=download_file_walkability,  # function to download file
                   process_file=process_file_walkability,   # function to process file
                   variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping)
  )

  external_data <- load_external_data(external_data_name_to_info_list)[["walkability"]]

  expect_equal(sum(is.na(external_data$estimate)), 0)  # shoud have no NA

  expect_equal(dim.data.frame(external_data), c(1985877, 3))
  expect_equal(pull(external_data[3,2]), "COUNTHU10")
  expect_equal(pull(external_data[98724,3]), 898)
  expect_equal(pull(external_data[813984,2]), "WORKERS")
  expect_equal(pull(external_data[919499,1]), "080050068564")
  expect_equal(pull(external_data[1594938,3]), 385.7149963)
  expect_equal(pull(external_data[1924930,3]), 7.833)
})


test_that("get_acmt_standard_array with both mrfei and walkability", {
  external_data_name_to_info_list <- list(
    mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
               download_file=download_file_mrefi,  # function to download file
               process_file=process_file_mrefi,    # function to process file
               geoid_type="Census Tract",
               variable_name_to_interpolate_by_sum_boolean_mapping=mrfei_variable_name_to_interpolate_by_sum_boolean_mapping
    ),

    walkability=list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                     download_file=download_file_walkability,  # function to download file
                     process_file=process_file_walkability,   # function to process file
                     geoid_type="Block Group",
                     variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping
    )
  )
  experiment_measures <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE, use_lower_resolution_geo_data = FALSE)

  expect_equal(dim(experiment_measures), c(689,2))
  expect_equal(filter(experiment_measures, names == "COUNTHU10")$values, 26433.73646)
  expect_equal(filter(experiment_measures, names == "WORKERS")$values, 24041.72925)
  expect_equal(filter(experiment_measures, names == "AC_LAND")$values, 2836.428925)
  expect_equal(filter(experiment_measures, names == "AC_UNPR")$values, 2416.972337)
  expect_equal(filter(experiment_measures, names == "NatWalkInd")$values, 16.80091)
})


test_that("get_acmt_standard_array with walkability", {
  external_data_name_to_info_list <- list(
  #mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
  #           download_file=download_file_mrefi,  # function to download file
  #           process_file=process_file_mrefi,    # function to process file
  #           geoid_type="Census Tract"
  #),

  walkability=list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                   download_file=download_file_walkability,  # function to download file
                   process_file=process_file_walkability,   # function to process file
                   geoid_type="Block Group",
                   variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping
  )
  )
  experiment_measures <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE, use_lower_resolution_geo_data = FALSE)

  expect_equal(dim(experiment_measures), c(688,2))
  expect_equal(experiment_measures[[680, 1]], "COUNTHU10")
  expect_equal(experiment_measures[[680, 2]], 26433.73646)
  expect_equal(experiment_measures[[684, 2]], 3327.7233)
  expect_equal(experiment_measures[[685, 2]], 491.20537)
  expect_equal(experiment_measures[[687, 1]], "AC_UNPR")
})

test_that("get_acmt_standard_array with walkability", {
  external_data_name_to_info_list <- list(
    walkability=list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                     download_file=download_file_walkability,  # function to download file
                     process_file=process_file_walkability,   # function to process file
                     geoid_type="Block Group",
                     variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping

    )
  )
  experiment_measures <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, use_lower_resolution_geo_data = FALSE)

  expect_equal(dim(experiment_measures), c(688,2))
  expect_equal(filter(experiment_measures, names == "COUNTHU10")$values, 26433.73646)
  expect_equal(filter(experiment_measures, names == "TOTPOP10")$values,  54285.697147)
  expect_equal(filter(experiment_measures, names == "HH")$values, 24726.49032)
  expect_equal(filter(experiment_measures, names == "WORKERS")$values, 24041.729253)
  expect_equal(filter(experiment_measures, names == "AC_TOT")$values, 3327.723328)
  expect_equal(filter(experiment_measures, names == "AC_WATER")$values, 491.20537)
  expect_equal(filter(experiment_measures, names == "AC_LAND")$values, 2836.428925)
  expect_equal(filter(experiment_measures, names == "AC_UNPR")$values, 2416.972337)
  expect_equal(filter(experiment_measures, names == "NatWalkInd")$values, 16.80091)
})


test_that("get_acmt_standard_array with walkability and lower resolution", {
  external_data_name_to_info_list <- list(
    walkability=list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                     download_file=download_file_walkability,  # function to download file
                     process_file=process_file_walkability,   # function to process file
                     geoid_type="Block Group",
                     variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping
    )
  )

  experiment_measures_low_resolution <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE, use_lower_resolution_geo_data=TRUE)

  expect_equal(dim(experiment_measures_low_resolution), c(688,2))
  expect_equal(experiment_measures_low_resolution[[680, 1]], "COUNTHU10")
  expect_equal(filter(experiment_measures_low_resolution, names=='COUNTHU10')$values, 26447.61146)
  expect_equal(filter(experiment_measures_low_resolution, names=='AC_TOT')$values, 3329.916651)
  expect_equal(substr(as.character(filter(experiment_measures_low_resolution, names=='AC_WATER')$values), 0, 8), "491.3289")
  expect_equal(experiment_measures_low_resolution[[687, 1]], "AC_UNPR")
})
