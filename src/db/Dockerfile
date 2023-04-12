# Postgis
FROM mdillon/postgis

# remove invalid deb address from source file
RUN sh -c 'echo "#deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main 11" > /etc/apt/sources.list.d/pgdg.list'

# Configure utils
RUN apt-get update \
    && apt-get install -y wget unzip \
    && rm -rf /var/lib/apt/lists/*

# Make data dir
RUN mkdir -p /gisdata \
    && chmod 777 -R /gisdata

# Copy nation an state scripts
COPY ./load_data.sh /docker-entrypoint-initdb.d/

# Overwrite the default /postgis.sh file
COPY ./postgis.sh /docker-entrypoint-initdb.d/

## install fish, vim and nano
RUN apt-get install fish
RUN apt-get install nano
RUN apt-get install vim
