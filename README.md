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
```
external_data_name_to_info_list <- list(
  mrfei=list(download_file=download_file_mrefi,  # function to download the mRFEI data to workspace;
             vector_of_expected_downloaded_file_name=c("downloaded_mrfei.xls")    # the files the downloader is expected to download; ACMT will check if these files are downloaded in workspace/external_data before processing the files
             process_file=process_file_mrefi)   # function to process file to make them fit ACMT format
)
```

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

## References
ACMT: https://github.com/smooney27/ACMT  
Docker-ACMT (not fully dockerized): https://github.com/smooney27/docker-acmt  
Postgis-Docker (where we borrowed the geocoder): https://github.com/uwrit/postgis-docker  
Docker tutorials: https://docs.docker.com/get-started/  
mRFEI: https://www.cdc.gov/obesity/resources/reports.html
