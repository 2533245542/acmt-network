FROM rocker/verse:3.6.3
#FROM rocker/verse:3.6.1
#FROM rocker/verse:4.0.3
#FROM rocker/geospatial:4.0.3

#update sources.list for bullseye 
RUN sh -c 'echo "deb http://deb.debian.org/debian bullseye main contrib non-free\ndeb-src http://deb.debian.org/debian bullseye main contrib non-free\ndeb http://deb.debian.org/debian-security/ bullseye-security main contrib non-free\ndeb-src http://deb.debian.org/debian-security/ bullseye-security main contrib non-free\ndeb http://deb.debian.org/debian bullseye-updates main contrib non-free\ndeb-src http://deb.debian.org/debian bullseye-updates main contrib non-free\ndeb http://deb.debian.org/debian bullseye-backports main contrib non-free\ndeb-src http://deb.debian.org/debian bullseye-backports main contrib non-free" > /etc/apt/sources.list'

# Copy local files into app container
RUN mkdir home/rstudio/workspace
COPY workspace/ /home/rstudio/workspace
RUN chown -R rstudio /home/rstudio/workspace

# Install packages for running acmt
RUN apt-get update
#RUN apt-get install -y --allow-unauthenticated fish vim nano curl libudunits2-dev libgdal-dev
RUN yes | apt-get install libudunits2-dev
RUN yes | apt-get install libgdal-dev

RUN R -e "install.packages(c('geosphere', 'lwgeom', 'raster', 'sf', 'tidycensus', 'tigris', 'units', 'USAboundaries', 'reshape2', 'rgeos', 'osrm', 'leaflet'))"
RUN echo "setwd(\"/home/rstudio/workspace/\")" > ~/../home/rstudio/.Rprofile  #set up working diretory
ARG BYPASS_SSL
RUN echo BYPASS_SSL=$BYPASS_SSL > ~/../home/rstudio/.Renviron  #pass the flag for R to access
RUN chown -R rstudio ~/../home/rstudio/.Renviron  # let rstudio have permission to write .Renviron


# Set up the shiny app for acmt
# RUN R -e "install.packages(c('shiny', 'shinythemes', 'tidygeocoder'))"
# RUN yes | apt-get install gdebi-core
# RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb
# RUN yes | gdebi shiny-server-1.5.16.958-amd64.deb
# RUN git clone https://github.com/2533245542/acmt-shiny.git /srv/shiny-server/acmt_shiny
# RUN sed -i '2s/.*/run_as rstudio;/' /etc/shiny-server/shiny-server.conf
# RUN sed -i '17s#.*#path <- "http://172.17.0.1:5000/latlong?"#' /home/rstudio/workspace/GeocoderACMT.R
# RUN sed -i '49s#.*#acs_columns_url <- "http://172.17.0.1:7000/ACSColumns.csv"#' /home/rstudio/workspace/GeocoderACMT.R
# CMD shiny-server
