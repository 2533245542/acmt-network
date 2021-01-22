# Run ACMT alone
## Update: actually need to manually link the docker workspace to the local workspace, TBD
`acmt-network` has two components, geocoder and ACMT. If a user have only limited disk storage, the user might want to install ACMT only.

To install ACMT only (without the geocoder), copy the `acmt-network/ACMT` folder into `acmt-network/src/app/workspace` (then update files in newly generated `acmt-network/src/app/workspace/ACMT` based on your own research purpose, if necessary). 

Comment out the below lines in `GeocoderACMT.R` to make ACMT completely rely on files in `acmt-network/src/app/ACMT` and not download external files.
```
download.file(url = "http://sandbox.idre.ucla.edu/mapshare/data/usa/other/spcszn83.zip", destfile = "ACMT/spcszn83.zip")
unzip("ACMT/spcszn83.zip", exdir="ACMT")
```

```
download.file(url = "https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_500k.zip", destfile = "ACMT/cb_2017_us_county_500k.zip")
unzip("ACMT/cb_2017_us_county_500k.zip", exdir="ACMT")
```

```
download.file(url = acs_columns_2010_url, destfile = "ACMT/2010ACSColumns.csv")
```

In terminal, navigate to `acmt-network/src/app/` and execute `docker run`.

Open RStudio following steps the same as that of running ACMT with geocoder.

Run below codes to test if installation succeeds (no errors generated).

```
source("RefreshAPIKey.R")
source("GeocoderACMT.R")

# Test ACMT
measures <- get_acmt_standard_array(-122.333, 47.663, 2000, year=2010)  
```