
FROM nginx
RUN apt-get update
RUN apt-get install wget unzip -y
COPY index.html /usr/share/nginx/html/
EXPOSE 80
CMD ["nginx","-g","daemon off;"]

ADD get_shapefiles.sh /
RUN echo "Getting shapefiles..."
RUN chmod +x /get_shapefiles.sh
RUN /get_shapefiles.sh

RUN echo "Copying ACMT Config Files"
COPY 2010ACSColumns.csv /usr/share/nginx/html/
COPY ACSColumns.csv /usr/share/nginx/html/