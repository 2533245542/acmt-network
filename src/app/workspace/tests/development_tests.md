# Performing unit testing for ACMT
To encourge reproducibility and increase the robustness of ACMT, we will develop a set of unit tests that checks for ACMT's correctness.

## Goal
Our goal is to make ACMT perform consistently in this Docker environment. 

Because Docker itself performs consistently across computer platforms, if ACMT perform consistently in this Docker environment, it would perform consistently across computer platforms.

Note that our goal is not to let ACMT itself perform the same on any computers because it would require considering many factors (package versions, R versions, operating systems, etc) and increase the workload. Whereas in `acmt-network`, our work is simpler because all these factors are fixed.

## Development schedule overview
### Phase 1
Develop the unit tests on the local computer.

The local computer's `local_GeocoderACMT.R` has been properly configured to make it bahave the same as `acmt-network`'s `GeocoderACMT.R`.

Thus, the tests that work on the local computer should work on `acmt-network` as well.

### Phase 2
Copy the unit tests into `acmt-network/src/app/workspace/tests`. Set the working directory to be `acmt-network/src/app/workspace/tests`, run the tests and make sure they pass.

## Phases
### Phase 1
On local computer, develop tests, modify the way to get ACS results in `local_GeocoderACMT.R` and add a function for handling missing ACS variables, and change `ACSColumns.csv`.
### Phase 2
1. Copy file `learnACMT/tests/test_acmt_network.R` to `acmt-network/src/app/workspace/tests/test_acmt_network.R`.

1.1. In `acmt-network/src/app/workspace/test_acmt_network.R`, change `source("local_GeocoderACMT.R")` to `source("GeocoderACMT.R")`.

2. Copy file `learnACMT/tests/tester.R` to `acmt-network/src/app/workspace/tests/test_acmt_network.R`.

3. Copy file `learnACMT/ACMT/ACSColumns.csv` to `acmt-network/src/files/ACSColumns.R` with replacement.

4. In `acmt-network/src/app/workspace/GeocoderACMT.R`, 
4.1 Add 
```
acs_columns_url = "http://host.docker.internal:7000/ACSColumns.csv"
download.file(url = acs_columns_url, destfile = "ACMT/ACSColumns.csv")
```

4.2 Add function `get_acs_results_for_available_variables()` 
```
get_acs_results_for_available_variables <- function (acs_var_names, state, county, year) {
  ### Remove the acs_var_names that are not available for the year and only return the available ones ###
  ### Return NA if error is not due to missing acs_var_names ###
  input_acs_var_names <- acs_var_names
  acs_results <- NA
  has_missing_variable_error <- TRUE
  while(has_missing_variable_error) {
    # Try to get acs results and if there is no error, we just proceed to the next step. If there is an error, we check if it is due to the missing variable; if it is, we remove the missing variable and repeat the loop; if it is not due to missing variable, we proceed to the next step. Thus, we proceed when there is no error or the error is not caused by missing variable; we continue the loop only when there is missing variable error.

    if (length(acs_var_names) == 0) {  # break when all variables are missing
      break
    }

    has_missing_variable_error <- tryCatch({  # note that codes in try is not inside a new function, just treat it as normal R code
      acs_results <- get_acs("tract", variables=acs_var_names, state=state, county=county, cache_table = T, census_api_key=CENSUS_API_KEY, geometry = F, keep_geo_vars = T, year=year)
      FALSE   # FALSE will be assigned to has_missing_variable_error; cannot use return here, otherwise the rest of the function will not execute
    }, error = function(condition) {  # note that in error handler, the codes are inside a new function; this is why we need <<- for assigning acs_var_names
      error_is_caused_by_missing_variable <- grepl(pattern = "Your API call has errors.  The API message returned is error: error: unknown variable '", x=condition$message, fixed = TRUE)
      if (error_is_caused_by_missing_variable) {  # remove the missing variable. Note that only one missing variable will be found by per get_acs run.
        front_trimmed_error_message <- sub("Your API call has errors.  The API message returned is error: error: unknown variable '", "", condition$message) # remove the former part
        code_of_the_missing_variable <- sub("E'.", "", front_trimmed_error_message)  # remove the latter part
        acs_var_names <<- acs_var_names[acs_var_names != code_of_the_missing_variable]
        print(paste("Removing missing variable:", code_of_the_missing_variable))
        return(TRUE)
      } else {
        return (FALSE)
      }
    })
  }

  # when there are missing variables, warn what they are, and even stop
  if (length(input_acs_var_names) > length(acs_var_names)) {
    warning(sprintf("Some ACS variables are missing for year %s", year))
    print("The missing ACS variables are:")
    print(setdiff(input_acs_var_names, acs_var_names))  # set minus latter
    print("The non-missing ACS variables are:")
    print(acs_var_names)
    if (length(acs_var_names) == 0) {
      stop(sprintf("All ACS variables are missing for year %s", year))
    }
  }

  return(acs_results)
}
```
4.3 In `get_count_variable_for_lat_long()`,
4.3.1 Replace

```
acs_results <- get_acs("tract", 
                       variables=acs_var_names, 
                       state=block_group_states[i], 
                       county=block_group_counties[i],
                       cache_table = T,
                       census_api_key=CENSUS_API_KEY,
                       geometry = F, keep_geo_vars = T, year=year)
```

with 

```
acs_results <- get_acs_results_for_available_variables( 
                       acs_var_names=acs_var_names, 
                       state=block_group_states[i], 
                       county=block_group_counties[i],
                       year=year)
```

4.3.2 Add 
```
    if (length(unique(acs_results$variable)) < length(acs_var_names)) {   # if missing variables were pruned, update acs_var_names to let it only include the available variables
      acs_var_names <- acs_var_names[acs_var_names %in% acs_results$variable]  # not assigning acs_results$variable directly to acs_var_names because although they are the same, the order of variables is different due to calling get_acs
    }
```

4.4 In `get_acmt_standard_array()`, add the below codes after calling `get_count_variable_for_lat_long()`.

```
  acs_unique_var_cols_contains_missing_variables <- (length(count_results$name) < length(acs_unique_var_cols))
  if (acs_unique_var_cols_contains_missing_variables) {  # get_acs_standard_columns again
    acs_info <- get_acs_standard_columns(year=year, codes_of_variables_to_get=count_results$name)
    acs_columns <- acs_info$acs_columns
    acs_proportion_names <- acs_info$acs_proportion_names
    acs_count_names <- acs_info$acs_count_names
    acs_unique_var_cols <- acs_info$acs_unique_var_cols
  }
```

4.5 In `get_acs_standard_columns()`, 

4.5.1 Change `get_acs_standard_columns <- function(year=2017) {` to `get_acs_standard_columns <- function(year=2017, codes_of_variables_to_get=NA) {`

4.5.2 Insert the below codes right after reading the ACS columns.
```
  if (!is.na(codes_of_variables_to_get[1])) {  # filter acs_columns by provided variables
    acs_columns <- acs_columns[acs_columns$acs_col %in% codes_of_variables_to_get, ]
  }
```

## Usage
See `tests/README.md`.

## Tests
### geocoder
### state_plane_zones
Make sure the data is fixed and is as expected.
### counties
Make sure the data is fixed and is as expected.
### state_proj (loaded from USAboundaries)
Make sure the data is fixed and is as expected.
### get_projection_for_lat_long
### get_point_buffer_for_lat_long
### get_statecounty_tracts
### get_acs_results_for_available_variables
TODO add the existing tests to `tests` folder
### get_count_variable_for_lat_long
### get_acs_standard_columns
Make tests for 2010
TODO make tests for 2011 and after
### get_acmt_standard_array

## Development to reach acmt-network v0.0.3
Here we describe the edits we make to `acmt-network` after v0.0.2. We maintain a copy of `ACSColumns.csv` in the local computer and name it `ACSColumns_old.csv`.

### get_acs_standard_columns()
#### Description
This function does the following (not complete).

```
Creates a mapping from var_name to acs_col.
Creates the proportion names for each child `var_name` in the means of appending `proportion` at the end.
Creates the count names for each `var_name` by appending `count` at the end.
Gets the proportino pretty names for all child variable.
Gets the count pretty names.
...
```

This function returns the ACS columns. When year<2011, it uses `2010ACSColumns.csv`; when year>=2011, it uses `ACSColumns.csv`. However, it is not compatible with year >= 2011.


### ACSColumns.csv
#### Description
In `2010ACSColumns.csv`, there are 82 variables and 5 types of names. They are:

var_name: the abbreviated name for the variable
acs_col: the ACS code of the variable
universe_col: the ACS code of the variable's parent variable
pretty_name_count: the human readable name for the variable
pretty_name_proportion: the human readable name describing for which parent variable this variable is proportionated to

In `ACSColums.csv`, there are only 104 variables and 3 types of names. They are:
friendly_name: the abbreviated name for the variable
acs_col: the ACS code of the variable
universe_col: the ACS code of the variable's parent variable

#### Problem
One problem is the typo.
One problem is having the first type of name.
One problem is lacking two types of names.

#### Edits (3.)
##### Typo
Change `twelth_grade_no_diploma` to `twelfth_grade_no_diploma`.
##### Wrong type of name
We change the first type of name in `ACSColumns.csv` from `friendly_name` to `var_name`. 

We can see the first type of name for both `.csv`s, although have different names, have the same meaning. Because the `2010ACSColumsn.csv` is the default for running ACMT and ACMT is currently running well with `2010ACSColumns` but failing with `ACSColumns.csv`, so we assume the first type of name in `2010ACSColumns.csv` is correct and that of `ACSColumns.csv` is wrong. 

##### Lacks two types of names
Compared to `2010ACSColumns.csv`, `ACSColumns.csv` lacks two types of names, `pretty_name_count` and `pretty_name_proportion`. We need to add them manually.

We notice that some variables are the same between the two `.csv` files, we can make use of the overlaps to create some of the types of names. We have to do the non-overlapping ones manually.

To complete the columns, we search each row in `ACSColumns.csv` and find its correspondence in `2010ACSColumns.csv` and replace it; in case where no correspondence exists, we create our own.

Exist in `ACSColumns.csv` but not in `2010ACSColumns.csv`
```
same_house_last_year,B07201_002,
moved_within_msa,B07201_004,
moved_from_abroad,B07201_014,

pop_25_and_over,B15003_001,
no_education,B15003_002,B15003_001
pre_school,B15003_003,B15003_001
kindergarten,B15003_004,B15003_001
first_grade,B15003_005,B15003_001
second_grade,B15003_006,B15003_001
third_grade,B15003_007,B15003_001
fourth_grade,B15003_008,B15003_001
fifth_grade,B15003_009,B15003_001
sixth_grade,B15003_010,B15003_001
seventh_grade,B15003_011,B15003_001
eighth_grade,B15003_012,B15003_001
ninth_grade,B15003_013,B15003_001
tenth_grade,B15003_014,B15003_001
eleventh_grade,B15003_015,B15003_001
twelfth_grade_no_diploma,B15003_016,B15003_001
high_school_grad,B15003_017,B15003_001
ged_or_alt_diploma,B15003_018,B15003_001
some_college_less_than_1_year,B15003_019,B15003_001
some_college_1_year_or_more,B15003_020,B15003_001
associates_degree,B15003_021,B15003_001
bachelors_degree,B15003_022,B15003_001
masters_degree,B15003_023,B15003_001
professional_degree,B15003_024,B15003_001
doctoral_degree,B15003_025,B15003_001
```

We see that the first three does not have a parent variable, so we just set their `universe_col` and `pretty_name_proportion` to empty.
```
same_house_last_year,B07201_002,,People living in the same house they lived last year (count),
moved_within_msa,B07201_004,,People moved within the same metropolitan statistical area (count),
moved_from_abroad,B07201_014,,People moved from abroad (count),
```

The others have proportions. 
```
pop_25_and_over,B15003_001,,Residents aged 25 and older (count),
no_education,B15003_002,B15003_001,Residents with no education (count),Residents with no education (proportion of residents aged 25 and older)
pre_school,B15003_003,B15003_001,Residents with pre-school education (count),Residents with pre-school education (proportion of residents aged 25 and older)
kindergarten,B15003_004,B15003_001,Residents with kingdergarten education (count),Residents with kingdergarten education (proportion of residents aged 25 and older)
first_grade,B15003_005,B15003_001,Residents with first grade education (count),Residents with first grade education (proportion of residents aged 25 and older)
second_grade,B15003_006,B15003_001,Residents with second grade education (count),Residents with second grade education (proportion of residents aged 25 and older)
third_grade,B15003_007,B15003_001,Residents with third grade education (count),Residents with third grade education (proportion of residents aged 25 and older)
fourth_grade,B15003_008,B15003_001,Residents with fourth grade education (count),Residents with fourth grade education (proportion of residents aged 25 and older)
fifth_grade,B15003_009,B15003_001,Residents with fifth grade education (count),Residents with fifth grade education (proportion of residents aged 25 and older)
sixth_grade,B15003_010,B15003_001,Residents with sixth grade education (count),Residents with sixth grade education (proportion of residents aged 25 and older)
seventh_grade,B15003_011,B15003_001,Residents with seventh grade education (count),Residents with seventh grade education (proportion of residents aged 25 and older)
eighth_grade,B15003_012,B15003_001,Residents with eighth grade education (count),Residents with eighth grade education (proportion of residents aged 25 and older)
ninth_grade,B15003_013,B15003_001,Residents with ninth grade education (count),Residents with ninth grade education (proportion of residents aged 25 and older)
tenth_grade,B15003_014,B15003_001,Residents with tenth grade education (count),Residents with tenth grade education (proportion of residents aged 25 and older)
eleventh_grade,B15003_015,B15003_001,Residents with eleventh grade education (count),Residents with eleventh grade education (proportion of residents aged 25 and older)
twelfth_grade_no_diploma,B15003_016,B15003_001,Residents with twelfth grade education (count),Residents with twelfth grade education (proportion of residents aged 25 and older)
high_school_grad,B15003_017,B15003_001,Residents with high school graduation education (count),Residents with high school graduation education (proportion of residents aged 25 and older)
ged_or_alt_diploma,B15003_018,B15003_001,Residents with GED or Alt diploma (count),Residents with GED or Alt diploma (proportion of residents aged 25 and older)
some_college_less_than_1_year,B15003_019,B15003_001,Residents with some college education of less than 1 year, (count),Residents with some college education of less than 1 year, (proportion of residents aged 25 and older)
some_college_1_year_or_more,B15003_020,B15003_001,Residents with some college education of greater or equal to 1 year, (count),Residents with some college education of greater or equal to 1 year, (proportion of residents aged 25 and older)
associates_degree,B15003_021,B15003_001,Residents with associates degrees (count),Residents with associates degrees (proportion of residents aged 25 and older)
bachelors_degree,B15003_022,B15003_001,Residents with bachelors degrees (count),Residents with bachelors degrees (proportion of residents aged 25 and older)
masters_degree,B15003_023,B15003_001,Residents with masters degrees (count),Residents with masters degrees (proportion of residents aged 25 and older)
professional_degree,B15003_024,B15003_001,Residents with professional degrees (count),Residents with professional degrees (proportion of residents aged 25 and older)
doctoral_degree,B15003_025,B15003_001,Residents with doctoral degrees (count),Residents with doctoral degrees (proportion of residents aged 25 and older)
```


### get_count_variable_for_lat_long()
#### Description
When getting acs resulst, it is possible that some variables are missing for the given year. 

For example, a variable (B15003_001, pop_25_and_over) is queriable from 2012 but not 2011.
             
In more details, when year>=2011, ACMT finds the ACS measures corresponding to variables described in `ACSColumns.csv`. However, some variables in `ACSColumsn.csv` are missing in ACS for certain years. For example, B15003_001(pop_25_and_over), a variable ACMT should query when year>=2011, does not exist in the 2011 ACS result for King county, Washington.

#### Edits (4.3.1, 4.3.2)
We create a function that automatically prune the missing variables and return the ACS results for the available one. The function can replace 
```
acs_results <- get_acs("tract", 
                       variables=acs_var_names, 
                       state=block_group_states[i], 
                       county=block_group_counties[i],
                       cache_table = T,
                       census_api_key=CENSUS_API_KEY,
                       geometry = F, keep_geo_vars = T, year=year)
```

to 

```
acs_results <- get_acs_results_for_available_variables( 
                       acs_var_names=acs_var_names, 
                       state=block_group_states[i], 
                       county=block_group_counties[i],
                       year=year)
```

The added function will be added to the unit test as well.

Additionally, we update  `acs_var_names` to prune the missing variables.

```
    if (length(unique(acs_results$variable)) < length(acs_var_names)) {   # if missing variables were pruned, update acs_var_names to let it only include the available variables
      acs_var_names <- acs_var_names[acs_var_names %in% acs_results$variable]  # not assigning acs_results$variable directly to acs_var_names because although they are the same, the order of variables is different due to calling get_acs
    }

```

### get_acmt_standard_array()
#### Description
When missing variables are removed, the `acs_columns`, `acs_proportion_names`, `acs_count_names`, and `acs_unique_var_cols` should be updated to remove the missing variables as well. 

We detect if there are missing variables by looking at the results of `get_count_variable_for_lat_long`. If there is, call `get_acs_standard_columns` with only the available variables; we reassign `acs_columns`, `acs_proportion_names`, `acs_count_names`, and `acs_unique_var_cols`.

#### Edits (4.4)
Added the below codes after `get_count_variable_for_lat_long`.
```
  acs_unique_var_cols_contains_missing_variables <- (length(count_results$name) < length(acs_unique_var_cols))
  if (acs_unique_var_cols_contains_missing_variables) {  # get_acs_standard_columns again
    acs_info <- get_acs_standard_columns(year=year, codes_of_variables_to_get=count_results$name)
    acs_columns <- acs_info$acs_columns
    acs_proportion_names <- acs_info$acs_proportion_names
    acs_count_names <- acs_info$acs_count_names
    acs_unique_var_cols <- acs_info$acs_unique_var_cols
  }
```

### get_acs_standard_columns()
#### Description
To satisfy the requirement of only getting the results for a subset of variables. When `variables_to_get` is `NA`, get all variables; otherwise only get the specified ones.
#### Edits (4.5.1, 4.5.2)
Change from `get_acs_standard_columns <- function(year=2017) {` to `get_acs_standard_columns <- function(year=2017, codes_of_variables_to_get=NA) {`.

Insert the below codes right after reading the ACS columns.
```
  if (!is.na(codes_of_variables_to_get[1])) {  # filter acs_columns by provided variables
    acs_columns <- acs_columns[acs_columns$acs_col %in% codes_of_variables_to_get, ]
  }

```