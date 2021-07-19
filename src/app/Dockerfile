FROM rocker/verse:3.6.1
#FROM rocker/verse:4.0.3
#FROM rocker/geospatial:4.0.3

# Copy local files into app container
RUN mkdir home/rstudio/workspace
COPY workspace/ /home/rstudio/workspace
RUN chown -R rstudio /home/rstudio/workspace

# Install packages for running acmt
RUN apt-get update
RUN yes | apt-get install libudunits2-dev
RUN yes | apt-get install libgdal-dev
RUN R -e "install.packages(c('geosphere', 'lwgeom', 'raster', 'sf', 'tidycensus', 'tigris', 'units', 'USAboundaries', 'reshape2', 'rgeos', 'osrm', 'leaflet'))"
RUN echo "setwd(\"/home/rstudio/workspace/\")" > ~/../home/rstudio/.Rprofile  #set up working diretory
RUN yes | apt-get install curl
RUN yes | apt-get install fish

# Set up the shiny app for acmt
#RUN R -e "install.packages(c('shiny', 'shinythemes', 'tidygeocoder'))"
#RUN yes | apt-get install gdebi-core
#RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.16.958-amd64.deb
#RUN yes | gdebi shiny-server-1.5.16.958-amd64.deb
#RUN git clone https://github.com/2533245542/acmt-shiny.git /srv/shiny-server/acmt_shiny

#CMD ["nohup", "shiny-server", "&"]
#CMD ["shiny-server"]
