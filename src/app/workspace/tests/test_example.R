# if called by test_file, the working directory should be workspace/ (at the same level as RefreshAPIKey.R); otherwise comment this line
setwd("../")
options(stringsAsFactors = FALSE)  # it is default to be FALSE in R4.0 but TRUE in earlier R versions

library(testthat)

test_that("Examples should run without error", {
  # Example is not functions. It is self-contained and does not output things. So it is reasonable that it contains tests inside
  # We just need to ensure that it runs without error
  # The corretness of ACMT is based on other tests
  expect_error(rmarkdown::render("examples/population_density_over_distance.Rmd", "html_document"), regexp = NA)
  expect_error(rmarkdown::render("examples/using_external_data.Rmd", "html_document"), regexp = NA)
})