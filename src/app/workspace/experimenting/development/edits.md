# Edits
We describe the edits we make to the original `ACMT` codes here.

## get_count_variable_for_lat_long()
At the end of this function, `result <- lapply(acs_var_names, function(x) { suppressWarnings(st_interpolate_aw(population[,x], point_buffer, extensive=T)[[2]])})` is  performed to get the context measurement of each ACS variable and the census tract. However, this line of code uses position index `[[2]]` to extract the context measurement value. We know that with a more recent version of `sf`, the position of the context measure value is changed to `[[1]]`, making the code break. To avoid this, we use variable name to extract the context measurement value. We change the previous line to `result <- lapply(acs_var_names, function(x) { suppressWarnings(st_interpolate_aw(population[,x], point_buffer, extensive=T)[[x]])})`

## get_statecounty_tracts()
In `water <- st_union(st_as_sf(area_water(state = state, county = county)))`, `area_water` is called without specifying the `year` argument; it turns out that the default for `year` changes with `tigris` package version. Thus, we add the `year` argument when calling this function. We change the code to `water <- st_union(st_as_sf(area_water(state = state, county = county, year=year)))` 

We also change `tracts <- st_as_sf(tracts(state = state, county = county, year=2017))` to `tracts <- st_as_sf(tracts(state = state, county = county, year=year))` so the year of tract geometries to use is dependent on the function argument of `get_statecounty_tracts`.

### Notes
In `tracts <- st_as_sf(tracts(state = state, county = county, year=2017))`, the `year` argument is fixed to `2017` regardless or the arguments for `get_statecounty_tracts`. We let the arguments of `get_statecounty_tracts` decdie the `year` value for `tracts()`. This change makes sense because `area_water` and `tracts` will do set operations with each other and they should be from the same year. 

Besides, if we decide to use the fixed `year`'s census tract geometries regardless of the input to `get_acmt_standard_array(year=...)`, we can do so by setting our desired value as default in `get_statecounty_tracts <- function(state, county, year=desired value) {...}` and avoid supplying a `year` argument when calling `get_statecounty_tracts()` (e.g. `get_statecounty_tracts(state=block_group_states[i], county=block_group_counties[i])`). This set up has already been made in the original `ACMT` code (default to use 2017) and no edit is needed.

## get_acmt_standard_array()
In the beginning, add `if (year < 2010 | year > 2020) {stop("Year must be in between 2010 and 2020 (inclusive)")}`. This applies the constraint to `year`. `year` only determines which year's ACS data we use. This is necessary because we use cencus tract geometries and state plane zone projections from 2017. If `year` is too far away, the context measurement retreived might be off.

```now
143                       commute_start_11_12_count 4.881178e+02
144                      commute_start_12_4pm_count 2.786276e+03
145                      commute_start_4pm_on_count 1.553098e+03
146                      male_pop_25_and_over_count 1.692306e+04
147                     male_high_school_grad_count 1.618724e+03
148                     male_bachelors_degree_count 6.771664e+03
149                    female_pop_25_and_over_count 1.494082e+04
150                   female_high_school_grad_count 9.448503e+02
151                   female_bachelors_degree_count 6.308992e+03
```

## get_count_variable_for_lat_long()
We replace `tracts_for_state_county[,acs_var_names] = acs_results_wide[,acs_var_names]` with `tracts_for_state_county <- left_join(x=tracts_for_state_county, y=acs_results_wide, by="GEOID")`. 

### Notes
In this function, we need to find the census tract geometries for each county, and the census tract ACS results for each county. We need to pair the census tract geometries to their respective ACS results. So later we can find the intersection between the user-defined sampling area and the census tract, as well as computing the proportionalized ACS results at the same time. In the original `ACMT` code, it is assumed that the census tract geometries (a dataframe where each row stores the geometry of a census tract) are naively aligned with census tract ACS results (a dataframe where each row stores the ACS result of a census tract). Thus, the ACS results is directly assigned to the geometries. However, two dataframes ordered differently, and the only way to align them is to join them on `key=GEOID`. Also, because we excluded water area from the census tract geometries, the cencus tract geometry dataframe will have less rows than the ACS result dataframe. So we should use left join with census tract geometry on the left.
