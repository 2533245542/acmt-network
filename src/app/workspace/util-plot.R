#' Plot a geographical region with background map
#'
#' Returns a ggplot object.
#'
library("leaflet")
plot_buffer_with_background <- function (buffer, longitude, latitude) {
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=longitude, lat=latitude) %>%
    addPolygons(data = buffer)
  return(m)
}
