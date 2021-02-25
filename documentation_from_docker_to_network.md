# Overview of `app`
`app` is built using the `rocker` image `rocker/verse:3.6.1`. This image contains `tidyverse`, `Tex` packages and a web-based RStudio application. Based on the `Dockerfile`, `app` creates an OS(docker container) where ACMT runs. The 'workspace' folder contains files needed for running ACMT.

It is estimated that an hour of set-up time is needed.

# Details
`rocker/verse:3.6.1` contains the basic OS that we need for running ACMT, but we still need to install the OS software and R packages. For the operating systems, we install `libudunits2-dev` and `libgdal-dev`; they are needed for installing R packages `geosphere`, `lwgeom`, `raster`, `sf`, `tidycensus`, `tigris`, `units`, and `USAboundaries`.

## Running `app` individually
When we need to make adjustment to the OS, we can run `app` individually with the following instructions.

In terminal, go to `src/app`.

Run `docker build -t app .` to build the docker image for `app`, following the instructions specified in `Dockerfile`.

Run `docker run --rm -v /Users/wzhou87/Desktop/epiProject/docker-acmt/src/app/workspace:/home/rstudio/workspace -e PASSWORD=acmtPassword -p 8787:8787 app` to create a container from the  `app` image we just built.

Open a browser in your local computer and go to `localhost:8787`. In the log-in window, enter `rstudio` for username and `acmtPassword` for password. 

The RStudio opened in the browser is now connected from the docker container you just created.

## Migrating from docker-acmt to acmt-network
`docker-acmt` is a project to make ACMT more portable. It creates a docker network for data querying and it runs the `ACMT` R scripts on a local computer to query data (see `https://github.com/smooney27/docker-acmt`). The disavantage of this method is that, although the Docker network is isolated from the local computer, the R scripts are not. Now, we have incorporated the running of the R scripts into the Docker network (`docker-acmt`) as well and created `acmt-network`, a fully isolated environment for running `ACMT`. We achieve this by creating a Docker container that runs the R scripts and connects the container to the original docker network.

### Technical details
When the R scripts are run in a Docker container, the original way of querying data: pinging something like `http://localhost:5433/` failed. To fix this, we made a small tweak by adding the `internal` to the URL and changed it to `http://host.docker.internal:5433`. The `host.docker.internal` replaces `localhost` and allow the `app` container query other containers in `docker-acmt` as if the `app` container was run locally like before.

# Misallaneious
Recording some codes used during development.
```
curl http://localhost:5433/latlong?q=1410+NE+Campus+Parkway%2c+Seattle%2c+WA+98195
curl http://localhost:8787
curl http://host.docker.internal:5000
curl http://localhost:5000
```