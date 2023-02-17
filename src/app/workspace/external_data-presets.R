source("external_data-file_downloader_and_processor.R")

#' interpolatable data

mrfei_variable_name_to_interpolate_by_sum_boolean_mapping <- c(FALSE)
names(mrfei_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("mRFEI")
external_data_presets_mrfei <- list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
                                    expected_processed_file_name='processed_mrfei.csv',
                                    download_file=download_file_mrefi,  # function to download file
                                    process_file=process_file_mrefi,   # function to process file
                                    geoid_type="Census Tract",
                                    variable_name_to_interpolate_by_sum_boolean_mapping=mrfei_variable_name_to_interpolate_by_sum_boolean_mapping
)

walkability_variable_name_to_interpolate_by_sum_boolean_mapping <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  FALSE)
names(walkability_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("COUNTHU10", "TOTPOP10", "HH", "WORKERS", "AC_TOT", "AC_WATER", "AC_LAND", "AC_UNPR", "NatWalkInd")
external_data_presets_walkability <- list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                                          expected_processed_file_name='processed_walkability.csv',
                                          download_file=download_file_walkability,  # function to download file
                                          process_file=process_file_walkability,   # function to process file
                                          geoid_type="Block Group",
                                          variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping
)

food_access_variable_name_to_interpolate_by_sum_boolean_mapping <- c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
names(food_access_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("Urban", "POP2010", "OHU2010", "GroupQuartersFlag", "NUMGQTRS", "PCTGQTRS", "LILATracts_1And10", "LILATracts_halfAnd10", "LILATracts_1And20", "LILATracts_Vehicle", "HUNVFlag", "LowIncomeTracts", "PovertyRate", "MedianFamilyIncome", "LA1and10", "LAhalfand10", "LA1and20", "LATracts_half", "LATracts1", "LATracts10", "LATracts20", "LATractsVehicle_20", "LAPOP1_10", "LAPOP05_10", "LAPOP1_20", "LALOWI1_10", "LALOWI05_10", "LALOWI1_20", "lapophalf", "lapophalfshare", "lalowihalf", "lalowihalfshare", "lakidshalf", "lakidshalfshare", "laseniorshalf", "laseniorshalfshare", "lawhitehalf", "lawhitehalfshare", "lablackhalf", "lablackhalfshare", "laasianhalf", "laasianhalfshare", "lanhopihalf", "lanhopihalfshare", "laaianhalf", "laaianhalfshare", "laomultirhalf", "laomultirhalfshare", "lahisphalf", "lahisphalfshare", "lahunvhalf", "lahunvhalfshare", "lasnaphalf", "lasnaphalfshare", "lapop1", "lapop1share", "lalowi1", "lalowi1share", "lakids1", "lakids1share", "laseniors1", "laseniors1share", "lawhite1", "lawhite1share", "lablack1", "lablack1share", "laasian1", "laasian1share", "lanhopi1", "lanhopi1share", "laaian1", "laaian1share", "laomultir1", "laomultir1share", "lahisp1", "lahisp1share", "lahunv1", "lahunv1share", "lasnap1", "lasnap1share", "lapop10", "lapop10share", "lalowi10", "lalowi10share", "lakids10", "lakids10share", "laseniors10", "laseniors10share", "lawhite10", "lawhite10share", "lablack10", "lablack10share", "laasian10", "laasian10share", "lanhopi10", "lanhopi10share", "laaian10", "laaian10share", "laomultir10", "laomultir10share", "lahisp10", "lahisp10share", "lahunv10", "lahunv10share", "lasnap10", "lasnap10share", "lapop20", "lapop20share", "lalowi20", "lalowi20share", "lakids20", "lakids20share", "laseniors20", "laseniors20share", "lawhite20", "lawhite20share", "lablack20", "lablack20share", "laasian20", "laasian20share", "lanhopi20", "lanhopi20share", "laaian20", "laaian20share", "laomultir20", "laomultir20share", "lahisp20", "lahisp20share", "lahunv20", "lahunv20share", "lasnap20", "lasnap20share", "TractLOWI", "TractKids", "TractSeniors", "TractWhite", "TractBlack", "TractAsian", "TractNHOPI", "TractAIAN", "TractOMultir", "TractHispanic", "TractHUNV", "TractSNAP")
external_data_presets_food_access <- list(vector_of_expected_downloaded_file_name=c("downloaded_food_access.xlsx"),  # the files should be downloaded for mrfei
                                          expected_processed_file_name='processed_food_access.csv',
                                          download_file=download_file_food_access,  # function to download file
                                          process_file=process_file_food_access,   # function to process file
                                          geoid_type="Census Tract",
                                          variable_name_to_interpolate_by_sum_boolean_mapping=food_access_variable_name_to_interpolate_by_sum_boolean_mapping
)

no2_variable_name_to_interpolate_by_sum_boolean_mapping <- c(FALSE)
names(no2_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("NO2")
external_data_presets_no2 <- list(vector_of_expected_downloaded_file_name=c("downloaded_no2.xlsx"),  # the files should be downloaded for mrfei
                                  expected_processed_file_name='processed_no2.csv',
                                  download_file=download_file_no2,  # function to download file
                                  process_file=process_file_no2,   # function to process file
                                  geoid_type="Block Group",
                                  variable_name_to_interpolate_by_sum_boolean_mapping=no2_variable_name_to_interpolate_by_sum_boolean_mapping
)

o3_variable_name_to_interpolate_by_sum_boolean_mapping <- c(FALSE, FALSE)
names(o3_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("O3", "O3_stderr")
external_data_presets_o3 <- list(vector_of_expected_downloaded_file_name=c("2017_ozone_daily_8hour_maximum.txt"),  # the files should be downloaded for mrfei
                                  expected_processed_file_name='processed_o3.csv',
                                  download_file=download_file_o3,  # function to download file
                                  process_file=process_file_o3,   # function to process file
                                  geoid_type="Census Tract",
                                  variable_name_to_interpolate_by_sum_boolean_mapping=o3_variable_name_to_interpolate_by_sum_boolean_mapping
)

pm25_variable_name_to_interpolate_by_sum_boolean_mapping <- c(FALSE, FALSE)
names(pm25_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("PM25","PM25_stderr")
external_data_presets_pm25 <- list(vector_of_expected_downloaded_file_name=c("2017_pm25_daily_average.txt"),  # the files should be downloaded for mrfei
                                  expected_processed_file_name='processed_pm25.csv',
                                  download_file=download_file_pm25,  # function to download file
                                  process_file=process_file_pm25,   # function to process file
                                  geoid_type="Census Tract",
                                  variable_name_to_interpolate_by_sum_boolean_mapping=pm25_variable_name_to_interpolate_by_sum_boolean_mapping
)

#' point estimates
external_data_presets_call911 <- list(vector_of_expected_downloaded_file_name=c("downloaded_call911.csv"),
                                   expected_processed_file_name='processed_call911.csv',
                                   download_file=download_file_call911,
                                   process_file=process_file_call911
)

external_data_presets_crime_seattle <- list(vector_of_expected_downloaded_file_name=c("downloaded_crime_seattle.csv"),
                                      expected_processed_file_name='processed_crime_seattle.csv',
                                      download_file=download_file_crime_seattle,
                                      process_file=process_file_crime_seattle
)

external_data_presets_crime_boston <- list(vector_of_expected_downloaded_file_name=c("downloaded_crime_boston.csv"),
                                            expected_processed_file_name='processed_crime_boston.csv',
                                            download_file=download_file_crime_boston,
                                            process_file=process_file_crime_boston
)

external_data_presets_crime_chicago <- list(vector_of_expected_downloaded_file_name=c("downloaded_crime_chicago.csv"),
                                           expected_processed_file_name='processed_crime_chicago.csv',
                                           download_file=download_file_crime_chicago,
                                           process_file=process_file_crime_chicago
)
external_data_presets_crime_los_angeles <- list(vector_of_expected_downloaded_file_name=c("downloaded_crime_los_angeles.csv"),
                                            expected_processed_file_name='processed_crime_los_angeles.csv',
                                            download_file=download_file_crime_los_angeles,
                                            process_file=process_file_crime_los_angeles
)

external_data_presets_airbnb <- list(vector_of_expected_downloaded_file_name=c("downloaded_airbnb.csv"),
                                            expected_processed_file_name='processed_airbnb.csv',
                                            download_file=download_file_airbnb,
                                            process_file=process_file_airbnb
)

crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
names(crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping) = c("total_pop_2022", "CRMCYTOTC","CRMCYPERC","CRMCYMURD","CRMCYRAPE","CRMCYROBB", "CRMCYASST", "CRMCYPROC", "CRMCYBURG",  "CRMCYLARC", "CRMCYMVEH") 
external_data_presets_crimerisk <- list(vector_of_expected_downloaded_file_name=c("raw_crimerisk.csv"), ## data not downloaded, so it will be in the Inspace folder
                                        download_file=download_file_crimerisk,
                                        process_file=process_crimerisk,
                                        geoid_type="Block Group",
                                        variable_name_to_interpolate_by_sum_boolean_mapping=crimerisk_variable_name_to_interpolate_by_sum_boolean_mapping 
)

sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping = c(TRUE, TRUE, TRUE)
names(sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping) = c('total_num', 'total_crosswalk', 'total_sidewalk')
external_data_presets_sidewalk <- list(vector_of_expected_downloaded_file_name=c("downloaded_sidewalk.csv"),
                                        download_file=NULL,
                                        process_file=process_sidewalk,
                                        geoid_type="Census Tract",
                                        variable_name_to_interpolate_by_sum_boolean_mapping=sidewalk_variable_name_to_interpolate_by_sum_boolean_mapping 
)


places_variable_name_to_interpolate_by_sum_boolean_mapping_2018 = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
names(places_variable_name_to_interpolate_by_sum_boolean_mapping_2018) = c("total_pop_2010", "ACCESS2_CrudePrev", "ARTHRITIS_CrudePrev",
                                                               "BINGE_CrudePrev", "BPHIGH_CrudePrev", "BPMED_CrudePrev", "CANCER_CrudePrev",   
                                                               "CASTHMA_CrudePrev","CERVICAL_CrudePrev", "CHD_CrudePrev", "CHECKUP_CrudePrev",   
                                                               "CHOLSCREEN_CrudePrev","COLON_SCREEN_CrudePrev", "COPD_CrudePrev", "COREM_CrudePrev",       
                                                               "COREW_CrudePrev", "CSMOKING_CrudePrev", "DENTAL_CrudePrev", "DEPRESSION_CrudePrev",  
                                                               "DIABETES_CrudePrev", "GHLTH_CrudePrev", "HIGHCHOL_CrudePrev", "KIDNEY_CrudePrev",      
                                                               "LPA_CrudePrev", "MAMMOUSE_CrudePrev", "MHLTH_CrudePrev", "OBESITY_CrudePrev",     
                                                               "PHLTH_CrudePrev", "SLEEP_CrudePrev", "STROKE_CrudePrev", "TEETHLOST_CrudePrev")

places_variable_name_to_interpolate_by_sum_boolean_mapping = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                                                    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
names(places_variable_name_to_interpolate_by_sum_boolean_mapping) = c("total_pop_2010", "ACCESS2_CrudePrev", "ARTHRITIS_CrudePrev",
                                                                           "BINGE_CrudePrev", "BPHIGH_CrudePrev", "BPMED_CrudePrev", "CANCER_CrudePrev",   
                                                                           "CASTHMA_CrudePrev","CERVICAL_CrudePrev", "CHD_CrudePrev", "CHECKUP_CrudePrev",   
                                                                           "CHOLSCREEN_CrudePrev","COLON_SCREEN_CrudePrev", "COPD_CrudePrev", "COREM_CrudePrev",       
                                                                           "COREW_CrudePrev", "CSMOKING_CrudePrev", "DENTAL_CrudePrev", "DEPRESSION_CrudePrev",  
                                                                           "DIABETES_CrudePrev", "GHLTH_CrudePrev", "HIGHCHOL_CrudePrev", "KIDNEY_CrudePrev",      
                                                                           "LPA_CrudePrev", "MAMMOUSE_CrudePrev", "MHLTH_CrudePrev", "OBESITY_CrudePrev",     
                                                                           "PHLTH_CrudePrev", "SLEEP_CrudePrev", "STROKE_CrudePrev", "TEETHLOST_CrudePrev")


external_data_presets_places<- list(vector_of_expected_downloaded_file_name=c("downloaded_places2017.csv", "downloaded_places2018.csv"),
                                    expected_processed_file_name=c('processed_places.csv'),
                                    download_file=NULL,
              process_file=NULL,
              geoid_type="Census Tract",
              variable_name_to_interpolate_by_sum_boolean_mapping=places_variable_name_to_interpolate_by_sum_boolean_mapping
)


