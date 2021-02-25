# ACMT-Network

## Overview
ACMT-Network takes advantage of Docker and provides an isolated environment for running ACMT. Users can run ACMT in a web-based RStudio connected to the isolated environment and can conveniently move documents back and forth between the isolated environment and their local computer.

## Prerequisites
Docker installed
7 GB of storage space

## Installation
Download this repository using `git clone`. 

Enter the `acmt-network` folder.

Configure the `.env` file to specify the states you want to download geocoding files for, and the year these files are published.
 
Run `docker-compose up` in terminal. When installing this tool for the first time, it could take an hour to complete (downloading files for more states takes longer).

After `docker-compose up` is run to complete (usually when the terminal shows a message saying that the database is ready to accept connection `db_1     | 2020-12-25 23:11:41.680 UTC [1] LOG:  database system is ready to accept connections`), leave the terminal running and open `http://localhost:8787/` in a local browser. 

When asked to enter username and password, enter `rstudio` for username and `acmtPassword` for password. 

A web-based RStudio window will open and it is connected to the isolated environment for running ACMT. 

In the web-based RStudio, go into the `workspace` folder and set the `workspace` folder to working directory. The `workspace` folder is for exchanging data between the isolated environment and your local computer. The files in the isolated environment's `workspace` will be synchronized with your local computer's `acmt-network/src/app/workspace`.

Test if `acmt-network` is installed successfully by sourcing `TestDockerACMT.R`. If no error occurs, the installation succeeds.

You can pause running the environment by pressing `ctrl-c` in the terminal where you installed ACMT-Network, and the work will be saved.

To resume from where you paused the last time, run `docker-compose up` in the `acmt-network` folder.

In the above, `docker-compose up` does two things, installing ACMT-Network and creating an instance of ACMT-Network (the isolated environment we just ran). The ACMT-Network is installed in the form of Docker images and they can be found by clicking `Images` in the left panel of Docker Dashboard (`acmt-network_app`, `acmt-network_files`, `acmt-network_api` and `acmt-network_db`.). The instance of ACMT-Network is created in the form of a Docker container network and it can be found by clicking `Container/Apps` in the left panel of Docker Dashboard (`acmt-network`).

When pausing the environment we just created, we were simply pausing the ACMT-Network instance. As long as we did not remove the instance, we can always resume from where we left before. Nevertheless, we can always start a new instance by running `docker-compose up` in the `acmt-network` folder (if we have removed the instance we created before).


## Removing an ACMT-Network instance 
To remove an ACMT-Network instance, open Docker Dashboard, select `Containers/Apps` on the left panel; on the right panel, hover on the ACMT-Network instance you want to remove, click the stop and delete button to remove the instance. Most of the data created when running the ACMT-Network instance will be removed except for the files in `workspace` which corresponds to your local computer's `acmt-network/src/app/workspace`.

We also remove the volumes that are created when running an ACMT-Network instance. The volume is used for storing most of the data generated during the usage of the instance and it is not removed when the instance is removed. 

Type `docker volume ls -f dangling=true` in the terminal to find any dangling volumes, use `docker volume inspect` and `docker system df -v` to find the volumes that we want to remove, and use `docker volume prune` or `docker volume rm` to remove them.

## Uninstalling ACMT-Network
An ACMT-Network instance is created based on four Docker images that we created when installing ACMT-Network. To remove these images, in Docker Dashboard, select `Images` on the left panel; on the right panel, remove `acmt-network_app`, `acmt-network_files`, `acmt-network_api` and `acmt-network_db`.


## Reinstall after editing ACMT-Network
If you edited ACMT-Network (changed `Dockerfile`s, `docker-compose.yml`, added files to `src/files` etc.), you might want to reinstall and instantiate ACMT-Network to see the changes. This can be done by first removing the previous ACMT-Network instance and then typing `docker-compose up --build` in the terminal. It rebuilds the Docker images and creates a new ACMT-Network instance.

## ACMT Usage
###### Function:

`get_acmt_standard_array(long, lat, year=2017)`

###### Arguments:

`long`, `lat`: Defines the centroid (longitude, latitude) of the sampling area

`radius_meters`: Defines the radius of the sampling area (in meters)

`year`: The (end) year of American Community Survey data to use. Values are limited from 2010 to 2020 (inclusive). The version of other data (plane zone geometries, plane zone projection strings, etc ) are fixed to a pre-defined year.

`external_data_name_to_info_list=external_data_name_to_info_list`: Other than ACS measures, users can also provide context measures of their choice. This requires passing an `external_data_name_to_info_list`. `external_data_name_to_info_list` let ACMT know how to download and process the user-provided context measures to the ACMT-like format. 


Below is an example of a `external_data_name_to_info_list` that uses CDC's Food Environment Index (mRFEI) as ACMT's additional context measures. 

See `external_data-file_downloader_and_processor.R` for the details of implementing your own `download_file` and `process_file` function

Note: when the downloaded file is large (>100MB), it is highly recommended to create the processed file outside of ACMT-network and then drag it into ACMT-network because it is slow for Docker to download and process such a large file internally.
```
external_data_name_to_info_list <- list(
  mrfei=list(download_file=download_file_mrefi,  # function to download the mRFEI data to workspace;
             vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls")    # the files the downloader is expected to download; ACMT will check if these files are downloaded in workspace/external_data before processing the files
             process_file=process_file_mrefi,   # function to process file to make them fit ACMT format
             geoid_type="Census Tract"
  )
)
```

Currently type of supported GEOID: `Census Tract`, `Block Group`. See `https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html` for their definitions.

`fill_missing_GEOID_with_zero`: Sometimes a context measurement might be missing for the specific area, set this to `TRUE` to use 0 for that context measure; otherwise, the context measure will be returned as NA.


###### Examples:

```
# Get context measurement for the sampling area that centers at 
# longitude=-122.333 and latitude=47.663, and has a radius of 2000 meters. 
# Use ACS data from 2010 (end year).

get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters=2000, year=2010)

# Use external data source (the below uses mRFEI)
external_data_name_to_info_list <- list(
  mrfei=list(vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls"),  # the files should be downloaded for mrfei
             download_file=download_file_mrefi,  # function to download file
             process_file=process_file_mrefi)   # function to process file
)

measures_for_2013_with_external_data_with_fill_missing <- get_acmt_standard_array(long=-122.333, lat=47.663, radius_meters = 2000, year=2013, external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)


```


## Versions
Docker uses `Debian GNU/Linux 9.11 (stretch), GEOS 3.5.1, GDAL 2.1.2, PROJ 4.9.3` with the following session info.
```
R version 3.6.1 (2019-07-05)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 9 (stretch)

Matrix products: default
BLAS/LAPACK: /usr/lib/libopenblasp-r0.2.19.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] readxl_1.3.1        lwgeom_0.1-7        tigris_0.8.2
 [4] reshape2_1.4.3      raster_3.0-7        sp_1.3-2
 [7] USAboundaries_0.3.1 units_0.6-5         stringi_1.4.3
[10] geosphere_1.5-10    tidycensus_0.9.2    sf_0.8-0
[13] jsonlite_1.6        forcats_0.4.0       stringr_1.4.0
[16] dplyr_0.8.3         purrr_0.3.3         readr_1.3.1
[19] tidyr_1.0.0         tibble_2.1.3        ggplot2_3.2.1
[22] tidyverse_1.3.0     httr_1.4.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3         lubridate_1.7.4    lattice_0.20-38    class_7.3-15
 [5] assertthat_0.2.1   zeallot_0.1.0      plyr_1.8.5         R6_2.4.1
 [9] cellranger_1.1.0   backports_1.1.5    reprex_0.3.0       e1071_1.7-3
[13] pillar_1.4.2       rlang_0.4.2        lazyeval_0.2.2     uuid_0.1-2
[17] rstudioapi_0.10    rgdal_1.4-8        foreign_0.8-71     munsell_0.5.0
[21] broom_0.5.2        compiler_3.6.1     modelr_0.1.5       pkgconfig_2.0.3
[25] tidyselect_0.2.5   codetools_0.2-16   fansi_0.4.0        crayon_1.3.4
[29] dbplyr_1.4.2       withr_2.1.2        rappdirs_0.3.1     grid_3.6.1
[33] nlme_3.1-140       gtable_0.3.0       lifecycle_0.1.0    DBI_1.0.0
[37] magrittr_1.5       scales_1.1.0       KernSmooth_2.23-15 cli_2.0.0
[41] fs_1.3.1           xml2_1.2.2         generics_0.0.2     vctrs_0.2.0
[45] tools_3.6.1        glue_1.3.1         hms_0.5.2          colorspace_1.4-1
[49] maptools_0.9-9     classInt_0.4-2     rvest_0.3.5        haven_2.2.0
```

Experimentally, it is equivalent to using `MacOS, GEOS 3.8.1, GDAL 3.1.4, PROJ 6.3.1` with the following session info.
```
R version 4.0.3 (2020-10-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.2

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] sf_0.9-7     readxl_1.3.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.6         magrittr_2.0.1     units_0.6-7        tidyselect_1.1.0   R6_2.5.0          
 [6] rlang_0.4.10       dplyr_1.0.3        tools_4.0.3        grid_4.0.3         KernSmooth_2.23-17
[11] e1071_1.7-4        DBI_1.1.1          ellipsis_0.3.1     class_7.3-17       assertthat_0.2.1  
[16] tibble_3.0.5       lifecycle_0.2.0    crayon_1.3.4       purrr_0.3.4        vctrs_0.3.6       
[21] glue_1.4.2         compiler_4.0.3     pillar_1.4.7       cellranger_1.1.0   generics_0.1.0    
[26] classInt_0.4-3     pkgconfig_2.0.3   
```

## References
ACMT: https://github.com/smooney27/ACMT  
Docker-ACMT (not fully dockerized): https://github.com/smooney27/docker-acmt  
Postgis-Docker (where we borrowed the geocoder): https://github.com/uwrit/postgis-docker  
Docker tutorials: https://docs.docker.com/get-started/  
mRFEI: https://www.cdc.gov/obesity/resources/reports.html
