geocode("600 4th Ave, Seattle, WA 98104")
geocode("200 N Spring St, Los Angeles, CA 90012")
geocode("121 N LaSalle St, Chicago, IL 60602")
geocode("City Hall Park, New York, NY 10007")
geocode("1 City Hall Square #500, Boston, MA 02201")


city_hall_to_address_list <- list(
  seattle_city_hall="600 4th Ave, Seattle, WA 98104",
  los_angeles_city_hall="200 N Spring St, Los Angeles, CA 90012",
  chicago_city_hall="121 N LaSalle St, Chicago, IL 60602",
  new_york_city_hall="City Hall Park, New York, NY 10007",
  boston_city_hall="1 City Hall Square #500, Boston, MA 02201"
)
# for each address get lat/long
convert_address_to_lat_long <- function (city_hall_to_address_list) {
  city_hall_to_lat_long_list <- vector(mode="list", length=length(city_hall_to_address_list))
  names(city_hall_to_lat_long_list) <- names(city_hall_to_address_list)
  for (name in names(city_hall_to_address_list)){
    city_hall_to_lat_long_list[[name]] <- geocoder(city_hall_to_address_list[[name]])
  }
  return(city_hall_to_lat_long_list)
}

convert_address_to_lat_long(city_hall_to_address_list)
