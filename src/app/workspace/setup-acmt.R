BYPASS_SSL <- as.logical(Sys.getenv("BYPASS_SSL"))  # in most cases this should be false
if (BYPASS_SSL) {
  library(httr)
  httr::set_config(config(ssl_verifypeer=0L))
}

source("external_data-file_downloader_and_processor.R")
source("external_data-file_loader.R")
source("external_data-presets.R")
source("util-geocoding.R")
source("util-plot.R")

source("RefreshAPIKey.R")
source("GeocoderACMT.R")
source("gather_points.R")

