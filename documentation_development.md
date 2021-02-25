# Useful materials for learning Docker and R on Docker
Docker's own tutorial that comes with the Docker installation
https://colinfay.me/docker-r-reproducibility/
https://ropenscilabs.github.io/r-docker-tutorial/04-Dockerhub.html

# Overview
ACMT-Network takes advantage of Docker and provides an isolated environment for running ACMT. Users can run ACMT in a web-based RStudio connected to the isolated environment and can conveniently move documents back and forth between the isolated environment and their local computer.

# Prerequisites
Docker installed
7 GB of storage space

# Installation
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


# Removing an ACMT-Network instance 
To remove an ACMT-Network instance, open Docker Dashboard, select `Containers/Apps` on the left panel; on the right panel, hover on the ACMT-Network instance you want to remove, click the stop and delete button to remove the instance. Most of the data created when running the ACMT-Network instance will be removed except for the files in `workspace` which corresponds to your local computer's `acmt-network/src/app/workspace`.

We also remove the volumes that are created when running an ACMT-Network instance. The volume is used for storing most of the data generated during the usage of the instance and it is not removed when the instance is removed. 

Type `docker volume ls -f dangling=true` in the terminal to find any dangling volumes, use `docker volume inspect` and `docker system df -v` to find the volumes that we want to remove, and use `docker volumn prune` or `docker volumn rm` to remove them.

# Uninstalling ACMT-Network
An ACMT-Network instance is created based on four Docker images that we created when installing ACMT-Network. To remove these images, in Docker Dashboard, select `Images` on the left panel; on the right panel, remove `acmt-network_app`, `acmt-network_files`, `acmt-network_api` and `acmt-network_db`.


# Reinstall after editing ACMT-Network
If you edited ACMT-Network (changed `Dockerfile`s, `docker-compose.yml`, etc.), you might want to reinstall and instantiate ACMT-Network to see the changes. This can be done by first removing the previous ACMT-Network instance and then typing `docker-compose up --build` in the terminal. It rebuilds the Docker images and creates a new ACMT-Network instance.

# Code illustration
## For docker-ACMT
The tool contains two important elements, `Geocoder` and `ACMT`. We use `Geocoder` to represent an address as longitude and latitude values; and we use `ACMT` to extract the context measure of a year and an area, which is quantified by radius, for the longtitude and latitude value.

Given an address (e.g. 1959 NE Pacific Street, Seattle, WY 98195), `Geocoder` gets the longitude and latitude of the location. ACMT then uses the longitude and latitue, and an additional radius and year parameter to get the context measure.

The `GeocoderACMT.R` file contains all functions that enable `TestDockerACMT.R`.

To enable `Geocoder`, we need to download the required files. The procedure for downloading files are written in codes and we only need to edit several parameters. We go into the `docker-acmt` directory, open the `.env` file, and edited the `GEOCODER_STATES=<Your state abbreviations (e.g. WA,OR,CA)>` entry. We then run `docker-compose up` to create a docker network. The docker network will start the file downloads. After the downloads are finished, the docker network will automatically build a database for `ACMT` to query as well .

Below shows the terminal messages when `docker-acmt` set-up completes.

```
db_1     | PostgreSQL init process complete; ready for start up.
db_1     |
db_1     | 2020-11-21 00:56:33.201 UTC [1] LOG:  listening on IPv4 address "0.0.0.0", port 5432
db_1     | 2020-11-21 00:56:33.201 UTC [1] LOG:  listening on IPv6 address "::", port 5432
db_1     | 2020-11-21 00:56:33.207 UTC [1] LOG:  listening on Unix socket "/var/run/postgresql/.s.PGSQL.5432"
db_1     | 2020-11-21 00:56:33.244 UTC [3039] LOG:  database system was shut down at 2020-11-21 00:56:32 UTC
db_1     | 2020-11-21 00:56:33.260 UTC [1] LOG:  database system is ready to accept connections
```


## API-level illustration
### Original API workflow
This tool is a docker network project with three containers, `db`, `files` and `server`. `db` sets up the `GISDatabase` for querying the longtitude and latitude value from an address; `files` downloads the data needed for generating the context measurement; `server` sets up the API for  `GeocoderACMT.R` and `TestDockerACMT.R` to query to database.

### Modified API workflow
We added the fourth component `app` which is a dedicated OS for running `GeocoderACMT.R` and `TestDockerACMT.R`. `app` connects to the docker network that contains `db`, `files` and `server`. Thus, it uses the API of `server` internally to get context measurements.

We use `rocker` as the base docker image for `app` because it supports `RStudio` connections. We also set up a dedicated folder called `workspace` inside `src/app` such that the container can communicate with the local computer.


## For ACMT
We first download the git repository. Following the instruction in `README.md`, we download the three datasets and put them into the git repository folder; unzip each dataset.

We then obtain and edit the `census_api_key` in `RefreshAPIKey.R`.

We should also edit `ACMT.R` and specifically, the assignment command for `land_cover`, `counties` and `state_plane_zones`. We need to specify the correct path such that all three variables are created corretly. for `land_cover`, we need to edit `land_cover <- raster("<relative path to .img file in NLCD>")`; for `counties`, we need to edit `counties <- sf::st_read(dsn = "<the name of the download zip file for us_county_500k, but take away .zip>", layer = "<the .shp filename but take away .shp>"); for `state_plane_zones`, we need to edit `sf::st_read(dsn = "<the downloaded zip file name for USA_State_Plane_Zones_NAD83 and take away .zip>", layer = "<the .shp name but take away the .shp>")`.
`
 
We can then run `ACMTPlayground.R`.

## Edit history of docker-acmt in the transition from docker-acmt to acmt-network
### Setting working directory to the current folder
We changed ` setwd("C:\\Users\\sjm2186\\Downloads\\docker2\\docker-acmt\\") ` to `setwd(".")` such that the the current directory (`docker-acmt`) is the working directory for running `TestDockerACMT.R`.

### Adding census API key
`docker-acmt` does not contain `RefreshAPIKey.R` so we added `RefreshAPIKey.R`. `RefreshAPIKey.R` was used in `ACMT` to install the Census API key which can be obtained from `http://api.census.gov/data/key_signup.html`. Without this key installed, in `GeocoderACMT.R`, the `get_acs` function will raise the below error. We 

```
Error in get_acs("tract", variables = acs_var_names, state = block_group_states[i],  : 
  A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.
```
We made `TestDockerACMT.R` to source `RefreshAPIKey.R` to install the key beforehand running the tests. We have obtained a key and put it in `RefreshAPIKey.R`.

Correspondingly, we use the key by adding an extra argument `census_api_key=CENSUS_API_KEY` to function `get_acs` which is the file `GeocoderACMT.R`.

### Correct state abbreviation
In `TestDockerACMT.R`, We changed `geocode("1959 NE Pacific Street, Seattle, WY 98195")` to `geocode("1959 NE Pacific Street, Seattle, WA 98195")` such that the address is correct. Notice that the change is from `WY` to `WA`.  

### Download both WA and WY
In `.env`, we let the geocoder download data for both `WA` and `WY`. `location <- geocode("1959 NE Pacific Street, Seattle, WA 98195")` is not able to find a location when we are only downloading files from `WY`.

### Edit server
Because we changed the name of the project from `docker-acmt` to `acmt-network`, we should adaptively change the server initialization arguments such that the R Docker container (named as `acmt-network_app_1`) can find the server container (named as `acmt-network_db_1`) through their docker names.

In `src/server/flaskr/app/modules/postgis.py`, we changed `self.host = 'docker-acmt_db_1'` to `self.host = 'acmt-network_db_1'`.

## Edit history for ACMT
### URL for downloading US State Planes shapefile 
The old site has expired and we updated it to https://hub.arcgis.com/datasets/esri::usa-state-plane-zones-nad83 to download USA_State_Plane_Zones_NAD83.
### Sourcing ACMT.R 
The original one does not have quotation mark around "ACMT.R" which caused error and we fixed it to `source("ACMT.R")` 
### Paths for datasets
We changed 
`counties <- sf::st_read(dsn = "ACMT", layer = "cb_2017_us_county_500k")`
`state_plane_zones <- sf::st_read(dsn = "ACMT", layer = "USA_State_Plane_Zones_NAD83")`
to 
`counties <- sf::st_read(dsn = "cb_2017_us_county_500k", layer = "cb_2017_us_county_500k") `
`state_plane_zones <- sf::st_read(dsn = "USA_State_Plane_Zones_NAD83", layer = "619f40a4-2457-45d6-ba90-757f1b6eca3b2020313-1-i4fqtv.8dvf") `
such that the program can run correctly.
### Removal of unmentioned files
We remove the variable `mrfie`, `epa_walkability` such that the program can run correctly.
### Edit `get_acs_standard_columns`
We remove a folder level during the assignment of `acs_columns` to corretly load `2010ACSColumns.csv` and 'ACMT/ACSColumns.csv'.


# Code testing (tentative)
We apply integrity tests on  DockerACMT to ensure it runs on multiple platforms. 

One method we used is to upload it to DockerHub and run it in Play with Docker. DockerHub is the registry for storing docker images, and Play with Docker allows us to test our docker image on a remote machine that has never seen this image before. Both DockerHub and Play with Docker are provided by the Docker company.
# Misellaneous
```
docker-compose up
docker-compose up --build
docker-compose build
```