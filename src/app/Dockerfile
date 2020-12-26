FROM rocker/verse:3.6.1

# Make data dir
RUN mkdir home/rstudio/workspace
RUN apt-get update

## input yes if a y/n question is asked
RUN yes | apt-get install libudunits2-dev
RUN yes | apt-get install libgdal-dev
RUN R -e "install.packages(c('geosphere', 'lwgeom', 'raster', 'sf', 'tidycensus', 'tigris', 'units', 'USAboundaries'))"
RUN yes | apt-get install curl


## Copy nation an state scripts
#COPY ./load_data.sh /docker-entrypoint-initdb.d/
#
## Overwrite the default /postgis.sh file
#COPY ./postgis.sh /docker-entrypoint-initdb.d/