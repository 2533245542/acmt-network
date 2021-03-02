path <- "http://host.docker.internal:5000/latlong?"

geocode <- function(address) {
  request <- GET(url = path, query=list(q=address))
  latitude <- NA
  longitude <- NA
  if (request$status_code > 300) {
    print(sprintf("status code for address %s is %s", address, request$status_code))
  } else {
    response <- content(request, as = "text", encoding = "UTF-8")
    df <- fromJSON(response, flatten = TRUE)
    if (!is.null(df$lat)) { latitude <- as.numeric(as.character(df$lat)) }
    if (!is.null(df$long)) { longitude <- as.numeric(as.character(df$long)) }
  }
  return(list(latitude=latitude, longitude=longitude))
}

address <- "600 4th Ave, Seattle, WA 98104"
request <- GET(url = path, query=list(q=address))
print(request)

# geocode("600 4th Ave, Seattle, WA 98104")
# geocode("200 N Spring St, Los Angeles, CA 90012")
# geocode("121 N LaSalle St, Chicago, IL 60602")
# geocode("City Hall Park, New York, NY 10007")
# geocode("1 City Hall Square #500, Boston, MA 02201")



