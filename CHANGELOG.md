v0.0.1
=====
* User can choose to install either ACMT+geocoder or geocoder alone.

v0.0.2
=====
* Use variable name instead of the position integer to get a census tract's context measurement from the interpolated result, so `ACMT` works with different versions of `sf`.
* Add year argument to `area_water()` and `tract()` so they match each other, as well as controled by their caller's argument.
* Added year constraints, from 2010 to 2020. 
* User left join to assign ACS results to census tract's geometries. Joining on the GEOID key.

v0.0.3
=====
* Add unit testing.
* Now it works for year>=2011.

v0.0.4
=====
* Merge ACSColumns.csv and 2010ACSColumns.csv.
* Automatically remove variables when they are missing for a ACS year.
* Create an interface for adding user-provided context measurements.
* Add mRFEI data to context measurements.
* Update README for instructions of adding user-provided context measurements

v0.0.5
=====
* TODO Add national walkability index data.
* TODO Let external data handler able to handle different types of GEOID.


Future
=====
* Verify acmt-network on existing work
* Write user instructions
