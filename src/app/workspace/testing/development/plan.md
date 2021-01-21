# Make sure a point is mapped to the correct plane zone
Steve worried the function `get_acmt_standard_array` might not correctly map the given longitude and latitude values to the correct plane zone.

To cancel his worries, we need to develop tests that ensures that a selected set of longitude and latitude values maps to the correct plane zones.

To develop the tests, we need to 1) understand the codes 2) find from reality mappings between longitude and latitude values to plane zones.

We describe what we did to accomplish these steps in below.

Other potential issues are also described.
1. ACMT defines a zone by creating a set of circular points. If the distance between two continous points is too large (points too sparse), there could also be problems.

2. The point buffer is projected while the `counties` are not, there could be a problem.

## Code reading
### Function calling order:
get_acmt_standard_array
    get_acs_standard_columns
    get_count_variable_for_lat_long
        get_point_buffer_for_lat_long
            get_projection_for_lat_long
                st_point
                st_sfc
            st_transform
            st_buffer
        st_intersects
        loop
            get_statecounty_tracts
        st_interpolate_aw

### Description
#### state_plane_zones 
Use state_plane_zones to find which state zone the lat/long is at. 

Contains each plane zone's polygons and code.
#### state_proj
Use the found state zone to find the projection string.

Contains plane zone's code and projection string.

A variable from `USAboundaries` library. This data frame includes plane zone codes, state abbreviations, EPSG codes, and proj4 strings for projections from the State Plane Coordinate System.
#### counties
It contains each county's polygons. 

Use `counties` to intersect with `point_buffer` to find which counties' data we should download.

A dataframe where each row is a county, with information of its polygon buffer. Do something like `plot(counties[1, ]$geometry, main = counties[1, ]$NAME)` to plot the shape of a county.
```
counties <- sf::st_read(dsn="ACMT", layer="cb_2017_us_county_500k")
counties <- st_transform(counties, 4326)
```
#### state_list
A list of dataframes. Each dataframe (state) has each row as a census track.

#### get_acmt_standard_array()
`get_acmt_standard_array` is where we feed the locations and it returns ACMT measures.
It gets the context measures for each state-county pair, proportional to the point buffer.
We should know that, in `acs_columns`, variables have two types. One is the unique variable and the other is universal variable. An universal variable contains serveral unique variables. For example, a unique variable is the populatino of mens between 10 to 20; and the corresponding universal variable is the population of all mens.
This function also calculates the unique variable's context measure proportional to the universal variable.
It then returns a dataframe where each row is a context measure (including unique variable's and their proportion to their univerisla variable's)

#### get_acs_standard_columns()
It reads the `2010ACSColumns.csv` and `acs_info$acs_columns` can return a dataframe for the csv file.
`2010ACSColumns.csv` is a description of variables and their respective codes.
#### get_count_variable_for_lat_long()
Create a circular point buffer for the given location and radius. 
Finds the counties that are intersecting the point buffer. 
For each state-county pair, get the census tract and the respective context measures.
Combine them, and we have a dataframe where the rows are census tracts and columns are context measures.
Interpolate the dataframe with the point buffer and we have the propotionally aggregated context measures for the point buffer.
#### get_point_buffer_for_lat_long()
Get a circular buffer for the given point and radius (in meters).
#### get_projection_for_lat_long()
Gets the project string given a point. It finds the plane zone of a point and get the projection string of the plane zone. A projection string describes the transformation from raw long/lat values to true values.
#### st_point()
Wrap the location (long, lat) in a `Point` class.
#### st_sfc()
Add descriptive information (e.g. which coordinate system to use) for the `Point` class.

argument crs=4325:
It specifies which the coordinate system to use. 4325 corresponds to latitude/longitude coordinate system based on the Earth's center of mass, used by the Global Positioning System among others (from wikipedia).
#### st_transform()
Transform a point to a new system. The system tranformed into is specified by the `crs` argument -- could be either EPSG code, or proj4string.
#### st_buffer()
Create a circular buffer around a point.
#### st_intersects()
Given a simple feature and a simple feature class, finds which part of the simple feature class is overlapped by the simple feature. It returns the index of the intersecting rows (of the simple feature class).
#### get_statecounty_tracts()
Find the tracts for this year's state-county pair . The track does not contain the water area.
#### st_interpolate_aw()
Calculate a point buffer's measure proportional to the buffer's intersecting areas. Setting `extensive=TRUE` makes the point buffer's measure the sum of the proportional areas' measures.



## Discussion of points accuracies
We found from the ACMT example, by looking at the circular buffer created, a larger distance (probabably largest) between two longitudes could be 0.0014 (-122.3089 - (-122.3075) = 0.0014)) and between two latitudes could be 0.001 (47.65120 - 47.65026 = 0.001). From online we know a latitude equals to 1132000 meters, and the from the sparcity we calculated for the latitude before, the actual sparsity in distance could be 1132 meters. This is a relatively large distance and could cause problems.
```
-122.2814	47.65095
-122.2814	47.65001
-122.2815	47.64907
-122.2816	47.64813
-122.2819	47.6472
-122.2822	47.64629
-122.2825	47.64538
-122.283	47.64448
-122.2835	47.64361
-122.284	47.64275
-122.2847	47.64191
-122.2854	47.6411
-122.2862	47.64032
-122.287	47.63956
-122.2879	47.63883
-122.2888	47.63813
-122.2898	47.63747
-122.2908	47.63685
-122.2919	47.63626
-122.2931	47.63571
-122.2942	47.63521
-122.2955	47.63474
-122.2967	47.63432
-122.298	47.63394
-122.2993	47.63361
-122.3006	47.63333
-122.302	47.63309
-122.3033	47.6329
-122.3047	47.63276
-122.3061	47.63267
-122.3075	47.63262
-122.3089	47.63263
-122.3103	47.63269
-122.3116	47.63279
-122.313	47.63295
-122.3144	47.63315
-122.3157	47.6334
-122.317	47.6337
-122.3183	47.63404
-122.3196	47.63443
-122.3209	47.63486
-122.3221	47.63534
-122.3232	47.63586
-122.3243	47.63641
-122.3254	47.63701
-122.3264	47.63765
-122.3274	47.63832
-122.3283	47.63902
-122.3292	47.63976
-122.33	47.64052
-122.3308	47.64132
-122.3315	47.64214
-122.3321	47.64298
-122.3327	47.64384
-122.3331	47.64472
-122.3336	47.64562
-122.3339	47.64653
-122.3342	47.64745
-122.3344	47.64838
-122.3345	47.64932
-122.3346	47.65026
-122.3346	47.6512
-122.3345	47.65214
-122.3344	47.65308
-122.3341	47.65401
-122.3338	47.65493
-122.3335	47.65584
-122.333	47.65673
-122.3325	47.65761
-122.3319	47.65847
-122.3313	47.6593
-122.3306	47.66011
-122.3298	47.6609
-122.329	47.66166
-122.3281	47.66239
-122.3272	47.66308
-122.3262	47.66374
-122.3251	47.66437
-122.3241	47.66496
-122.3229	47.66551
-122.3217	47.66601
-122.3205	47.66648
-122.3193	47.6669
-122.318	47.66728
-122.3167	47.66761
-122.3154	47.66789
-122.314	47.66813
-122.3127	47.66832
-122.3113	47.66846
-122.3099	47.66855
-122.3085	47.6686
-122.3071	47.66859
-122.3057	47.66853
-122.3043	47.66843
-122.303	47.66827
-122.3016	47.66807
-122.3002	47.66782
-122.2989	47.66752
-122.2976	47.66718
-122.2964	47.66679
-122.2951	47.66636
-122.2939	47.66588
-122.2928	47.66536
-122.2916	47.6648
-122.2906	47.66421
-122.2895	47.66357
-122.2885	47.6629
-122.2876	47.66219
-122.2868	47.66146
-122.2859	47.66069
-122.2852	47.6599
-122.2845	47.65908
-122.2839	47.65824
-122.2833	47.65737
-122.2828	47.65649
-122.2824	47.6556
-122.2821	47.65468
-122.2818	47.65376
-122.2816	47.65283
-122.2814	47.65189
-122.2814	47.65095
```

## Interseting point buffer and counties: projection issue
In `get_count_variable_for_lat_long`, we intersect the point buffer with the `counties` to find out the counties that are covered or partially covered by the point buffer. However, the point buffer was projected before using a projection string (generated using `state_plane_zones`) while `counties` are not projected. 

Should we project `counties` as well?

## Why remove the water from get_statecounty_tracts()
In get_statecounty_tracts(), when getting the shape of tracts, we are not getting the full tracts. Instead, the tracts we got are removed of the water area. This can be reasonable exept we later use these tracts' pologons to interpolate (overlap) the point buffer, to generate the proportional measures for the point buffer. It seems removing the water area will just make the interpolation less accurate.


## Error edits
### get_statecounty_tracts()
Constants are passed to functions instead of variables. We changed `tracts <- st_as_sf(tracts(state = state, county = county, year=2017))` to `tracts <- st_as_sf(tracts(state = state, county = county, year=year))`

Edits made in `workspace/testing/copy_of_GeocoderACMT.R`

### Mismatch ACSColumns and plane zone years
To run ACMT, we need two types of files, ACSColumsn and plane zones. Currently, the year argument to `get_acmt_standard_array` only determines what year to use for ACSColumn, and the year for plane zone is fixed to 2017.

Also, in ACMT, it says we should use `ACMT/2010ACSColumns.csv` when year is smaller than 2011, and use `ACMT/ACSColumns.csv` otherwise; but ACMT does not provide `ACMT/ACSColumns.csv` so ACMT fails when we set year to greater or equal to 2011.

Supposely, the given working example is to use 2010 ACSColumns and 2010 plane zones; but ACMT is using 2010 ACSColumns and 2017 plane zones;

Supposely, we should be able to use any year's plan zone but we only coded ACMT to download the 2017 plane zone.

Possible explanations and further steps for this issue:
1. It is intended that 2017 plane zone version is enough and should be hard coded. If this is true, we still need to provide the `ACMT/ACSColumns.csv` so `get_acmt_standard_array` can handle year input greater or equal to 2011. We also need to revert the first error edit (the error edit was implemented in `copy_of_GeocoderACMT.R` so we should just revert to the  `copy_of_GeocoderACMT.R` in `acmt-network`).

2.1 User should be free to select which year of plane zone to use and the current fixed 2017 plane zone is a mistake; also, users must use ACSColumsn and plane zone from the same years (e.g. ACSColumns from 2010 and plane zone from 2010). If this is true, we should implement the first error edit (already implemented in `copy_of_GeocoderACMT.R`); we should create a function for downloading plane zone files from a given year. We should provide `ACMT/ACSColumns.csv`.

2.2 User should be free to select which year of plane zone to use and the current fixed 2017 plane zone is a mistake; also, users can use ACSColumsn and plane zone from seperate years (e.g. ACSColumns from 2010 and plane zone from 2017). If this is true, we should implement the first error edit (already implemented in `copy_of_GeocoderACMT.R`); we should create a function for downloading plane zone files from a given year. We should also create an extra argument for `get_acmt_standard_array` to specify the ACSColumn year; `get_acmt_standard_array` should use this argument to replace the current `year` argument when calling `get_acs_standard_columns`. We should provide `ACMT/ACSColumns.csv`.

```
Warning in proportion_vals[i] <- count_results$estimate[count_results$name ==  :
  number of items to replace is not a multiple of replacement length
```

## Debug edits
### get_count_variable_for_lat_long()
The `year` variable is not provided to the called function, thus making the called function use the variable's default value.
`tracts_for_state_county <- get_statecounty_tracts(state=block_group_states[i], county=block_group_counties[i])` to `tracts_for_state_county <- get_statecounty_tracts(state=block_group_states[i], county=block_group_counties[i], year=year)`

### get_acmt_standard_array()
We let `get_acmt_standard_array` return more informatino to help us confirm the correctness of ACMT. Instead of return just a dataframe, we let the function return the below list. 
```
acmt_result_list = list(
context_measurement=data.frame(names=c(acs_proportion_names, acs_count_names), values=c(proportion_vals, count_vals)),
longitude=long,
latitude=lat,
radius=radius_meters,
year=year,
acs_info=acs_info
)

```
We add an argument to `get_acmt_standard_array` and it is called `return_auxiliary_information=FALSE`. This argument is default to be false and `get_acmt_standard_array` returns a dataframe as usually; when it is set to true, it returns the list we described above.

### get_count_variable_for_lat_long()
TOOD: Should let this return extra information for debugging as well.

### get_count_variable_for_lat_long()
In `get_count_variable_for_lat_long`, `population` interpolates with `point_buffer` to generate the proportional measures.

However, `st_agr(population)` indicates that attribute-aggregate relationship is NA for all features. 

We might want to set the features to be `aggregate`.

### Reverse goecoding
Use reverse geocoding tools `revgeo` to confirm that the coordinate system is always correct?

### Not-projected point buffer
The circular point buffer might not have been projected so it is a ellisodal buffer (using long/lat coordinate system) instead of a circular one (using projected system in meters/other measures).
Geogrphic CRS: the origin is always the center of the earth; the units are in lat/long degrees.
Projected CRS: the origin changes; the units can be meters