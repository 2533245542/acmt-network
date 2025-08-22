
FROM nginx

# set up apt-get repository
RUN sh -c 'echo > /etc/apt/sources.list.d/pgdg.list'

RUN apt-get update
RUN apt-get install -y --allow-unauthenticated fish vim nano locales wget unzip

COPY index.html /usr/share/nginx/html/
EXPOSE 80
CMD ["nginx","-g","daemon off;"]

ADD get_shapefiles.sh /
RUN echo "Getting shapefiles..."
RUN chmod +x /get_shapefiles.sh
RUN /get_shapefiles.sh

RUN echo "Copying ACMT Config Files"
#COPY 2010ACSColumns.csv /usr/share/nginx/html/
COPY ACSColumns.csv /usr/share/nginx/html/

