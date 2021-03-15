FROM rocker/verse:3.6.1
#FROM rocker/verse:4.0.3
#FROM rocker/geospatial:4.0.3

# Make data dir
RUN mkdir home/rstudio/workspace
RUN apt-get update

## input yes if a y/n question is asked
RUN yes | apt-get install libudunits2-dev
RUN yes | apt-get install libgdal-dev
RUN R -e "install.packages(c('geosphere', 'lwgeom', 'raster', 'sf', 'tidycensus', 'tigris', 'units', 'USAboundaries', 'reshape2', 'rgeos'))"
#RUN R -e "install.packages(c('tidycensus', 'USAboundaries', 'reshape2'))"
RUN yes | apt-get install curl
RUN yes | apt-get install fish