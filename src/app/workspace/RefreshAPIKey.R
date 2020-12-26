library(tidycensus)

refresh_api_key <- function() {
  cat(file=stderr(), "Checking API key...")
  readRenviron("~/.Renviron")
  if (Sys.getenv("CENSUS_API_KEY") == "") {
    cat(file=stderr(), "Installing Census API key...")
    census_api_key('fd0f854ed2068a5c9c29c0f679403646cb4a4d30', install=T)
    cat(file=stderr(), "Installed Census API key")
  } else {
    cat(file=stderr(), "Census API key already installed")
  }
  readRenviron("~/.Renviron")  # if .Renviron did not exist, need to read it again
}
refresh_api_key()