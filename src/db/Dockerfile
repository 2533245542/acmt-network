# Postgis
FROM postgis/postgis:15-3.4

# install software
# Switch to root for package installs
USER root

# Update and install required software including postgis client tools
RUN apt-get update && \
    apt-get install -y --allow-unauthenticated fish vim nano locales wget unzip postgis postgresql-15-postgis-3 && \
    rm -rf /var/lib/apt/lists/*

# install software
RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG en_US.utf8
ENV LC_ALL en_US.utf8

# Make data directory with permissions
RUN mkdir -p /gisdata && chmod 777 -R /gisdata

# Copy nation an state scripts
COPY ./load_data.sh /docker-entrypoint-initdb.d/

# Overwrite the default /postgis.sh file
COPY ./postgis.sh /docker-entrypoint-initdb.d/

# Switch back to postgres user
USER postgres
