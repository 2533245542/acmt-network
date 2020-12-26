#!/bin/sh
cd /usr/share/nginx/html

echo "hello" > myhello.html

#wget https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_500k.zip  --mirror --reject=html
#cd /usr/share/nginx/html/www2.census.gov/geo/tiger/GENZ2017/shp/
#unzip cb_2017_us_county_500k.zip
#cp cb_2017_us_county_500k.* /usr/share/nginx/html
#cd /usr/share/nginx/html
#chmod +r cb_2017_us_county_500k.*

#wget https://opendata.arcgis.com/datasets/23178a639bdc4d658816b3ea8ee6c3ae_0.zip --reject=html
#unzip 23178a639bdc4d658816b3ea8ee6c3ae_0.zip

#mv 619f40a4-2457-45d6-ba90-757f1b6eca3b2020313-1-i4fqtv.8dvf.shp USA_State_Plane_Zones_NAD83.shp
#mv 619f40a4-2457-45d6-ba90-757f1b6eca3b2020313-1-i4fqtv.8dvf.shx USA_State_Plane_Zones_NAD83.shx
#mv 619f40a4-2457-45d6-ba90-757f1b6eca3b2020313-1-i4fqtv.8dvf.dbf USA_State_Plane_Zones_NAD83.dbf
#mv 619f40a4-2457-45d6-ba90-757f1b6eca3b2020313-1-i4fqtv.8dvf.prj USA_State_Plane_Zones_NAD83.prj

#wget http://sandbox.idre.ucla.edu/mapshare/data/usa/other/spcszn83.zip --reject=html
#unzip spcszn83.zip

#mv spcszn83.shp USA_State_Plane_Zones_NAD83.shp
#mv spcszn83.shx USA_State_Plane_Zones_NAD83.shx
#mv spcszn83.dbf USA_State_Plane_Zones_NAD83.dbf
#mv spcszn83.prj USA_State_Plane_Zones_NAD83.prj


# wget ftp://newftp.epa.gov/EPADataCommons/OP/WalkabilityIndex.zip
# cd /gisdata/newftp.epa.gov/EPADataCommons/OP/
# unzip WalkabilityIndex.zip

