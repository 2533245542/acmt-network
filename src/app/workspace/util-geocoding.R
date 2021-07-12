#' Get GEOID for a pair of latitude and longitude.
#'
#' Returns GEOID for 2020 geographies.
#' Note: This function might fail in the future dur to census.gov using non-2020 geographies. In that case, just change the 2020 to the appropriate year. To find the appropriate year, refer to https://github.com/walkerke/tigris/blob/master/R/geocode.R.
#'
get_geoid_for_lat_long <- function(lat, lon, geoid_type="Census Tract") {
  # adapted from tigris::append_geoid and tigris::call_geolocator_latlon
  benchmark<-"Public_AR_Current"
  vintage<-"Current_Current"

  # Build url
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"
  url <- paste0("x=", lon,"&y=", lat)
  benchmark0 <- paste0("&benchmark=", benchmark)
  vintage0 <- paste0("&vintage=", vintage, "&format=json")
  url_full <- paste0(call_start, url, benchmark0, vintage0)

  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  stopifnot("Error: more than one GEOID for this lat/long" = length(response$result$geographies$`2020 Census Blocks`[[1]]$GEOID) == 1)
  full_geoid <- response$result$geographies$`2020 Census Blocks`[[1]]$GEOID

  # Edit GEOID to the desired type
  if (geoid_type == 'County') {
    end <- 5
  } else if (geoid_type == 'Census Tract') {
    end <- 11
  } else if (geoid_type == 'Block Group') {
    end <- 12
  } else {
    end <- 15
  }
  edited_geoid <- substr(full_geoid, 1, end)

  return(edited_geoid)
}

#' Get GEOID for a pair of latitude and longitude without accessing public interset services to protect patient privacy.
#'
#' Returns GEOID for 2020 geographies.
#'
get_geoid_for_lat_long_annonymous <- function (latitude, longitude, geoid_type = "County") {
  # get the county where the point is at
  point_buffer <- get_point_buffer_for_lat_long(long=longitude, lat=latitude, radius_meters=1)
  index_of_intersecting_counties <- st_intersects(point_buffer, counties)
  intersecting_counties_fips <- unique(as.character(counties$GEOID[index_of_intersecting_counties[[1]]]))
  intersecting_counties_fips_state_codes <- substr(intersecting_counties_fips, 1, 2)
  intersecting_counties_fips_county_codes <- substr(intersecting_counties_fips, 3, 5)
  if (length(index_of_intersecting_counties) != 1) {
    stop("The given latitude and longtitude should be in one and only one county.")
  }

  # grab the geometries of the county
  geoid_to_geometry_dataframe <- get_geometries_of_a_county(state=intersecting_counties_fips_state_codes[[1]], county=intersecting_counties_fips_county_codes[[1]], geoid_type="Block Group")  # get the most detailed one and trim later

  # find which geometry the point is at
  index_of_intersecting_region <- st_intersects(st_transform(point_buffer, st_crs(geoid_to_geometry_dataframe)), geoid_to_geometry_dataframe)
  intersecting_region_geoid <- unique(as.character(geoid_to_geometry_dataframe$GEOID[index_of_intersecting_region[[1]]]))

  # process the geoid to the asked format
  if (geoid_type == 'County') {
    end <- 5
  } else if (geoid_type == 'Census Tract') {
    end <- 11
  } else if (geoid_type == 'Block Group') {
    end <- 12
  } else {
    end <- 15
  }
  edited_geoid <- substr(intersecting_region_geoid, 1, end)
  return(edited_geoid)
}

#' Do same as osrm 3.4.1 package does
#'
#'
adpated_osrmIsochrone <- function(loc, breaks = seq(from = 0, to = 60, length.out = 7), exclude = NULL, res = 30, returnclass = "sp", osrm.server = "https://routing.openstreetmap.de/", osrm.profile = "car") {
  options(osrm.server="https://routing.openstreetmap.de/")
  options(osrm.profile="car")
  testSf <- function(x){
    if (methods::is(x,"sf")){
      if (is.na(sf::st_crs(x))){
        stop(paste( "Your input (", quote(x), ") does not have a valid coordinate reference system.", sep=""), call. = F)
      }
      return(TRUE)
    }
    return(FALSE)
  }
  clean_coord <- function(x){
    format(round(as.numeric(x),5), scientific = FALSE, justify = "none",
           trim = TRUE, nsmall = 5, digits = 5)
  }
  sfToDf <- function(x){
    if (is.na(sf::st_crs(x))){
      stop( paste( "Your input (", quote(x), ") does not have a valid coordinate reference system.", sep=""), call. = F)
    }
    # transform to centroid and to wgs84
    if (methods::is(st_geometry(x), c("sfc_GEOMETRY", 'sfc_GEOMETRYCOLLECTION'))){
      x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
    }
    if (methods::is(st_geometry(x), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))){
      sf::st_geometry(x) <- sf::st_centroid(x = sf::st_geometry(x),
                                            of_largest_polygon = T)
    }
    x <- sf::st_transform(x = x, crs = 4326)
    coords <- sf::st_coordinates(x)
    # this function takes an sf and transforms it into a dataframe
    x <- data.frame(id = row.names(x),
                    lon = clean_coord(coords[,1]),
                    lat = clean_coord(coords[,2]),
                    stringsAsFactors = FALSE)
    return(x)
  }
  ## osrmIsochrone Utils
  isopoly <- function(x, breaks,
                      xcoords = "COORDX", ycoords = "COORDY", var = "OUTPUT"){
    
    # get initial min and max values
    vmin <- min(x[[var]], na.rm = TRUE)
    vmax <- max(x[[var]], na.rm = TRUE)
    breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks < vmax], vmax)))
    # data points to matrix
    m <- matrix(data = x[[var]], nrow = length(unique(x[[xcoords]])),
                dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]])))
    # compute isobands
    lev_low = breaks[1:(length(breaks)-1)]
    lev_high = breaks[2:length(breaks)]
    raw <- isoband::isobands(x = as.numeric(rownames(m)),
                             y = as.numeric(colnames(m)), z = t(m),
                             levels_low = lev_low,
                             levels_high = c(lev_high[-length(lev_high)],
                                             vmax + 1e-10))
    bands <- isoband::iso_to_sfg(raw)
    iso <- st_sf(id = 1:length(bands),
                 min = lev_low,
                 max = lev_high,
                 geometry = sf::st_sfc(bands),
                 crs = st_crs(x))
    iso[1,"min"] <- 0
    iso$center = iso$min + (iso$max - iso$min) / 2
    
    # invalid polygons mgmnt
    st_geometry(iso) <- st_make_valid(st_geometry(iso))
    
    
    if(methods::is(st_geometry(iso),c("sfc_GEOMETRYCOLLECTION", "sfc_GEOMETRY"))){
      st_geometry(iso) <-   sf::st_collection_extract(st_geometry(iso), "POLYGON")
    }
    # get rid of out of breaks polys
    iso <- iso[-nrow(iso),]
    return(iso)
  }
  rgrid <- function(loc, dmax, res){
    # create a regular grid centerd on loc
    boxCoordX <- seq(from = sf::st_coordinates(loc)[1,1] - dmax,
                     to = sf::st_coordinates(loc)[1,1] + dmax,
                     length.out = res)
    boxCoordY <- seq(from = sf::st_coordinates(loc)[1,2] - dmax,
                     to = sf::st_coordinates(loc)[1,2] + dmax,
                     length.out = res)
    sgrid <- expand.grid(boxCoordX, boxCoordY)
    sgrid <- data.frame(ID = seq(1, nrow(sgrid), 1),
                        COORDX = sgrid[, 1],
                        COORDY = sgrid[, 2])
    sgrid <- sf::st_as_sf(sgrid,  coords = c("COORDX", "COORDY"),
                          crs = st_crs(loc), remove = FALSE)
    return(sgrid)
  }
  ## osrmTable Utils
  durTableFormat <- function(res, src, dst){
    # extract distance table
    mat <- res$durations
    # From sec to minutes
    mat <- round(mat/(60), 1)
    # col and row names management
    dimnames(mat) <- list(src$id, dst$id)
    return(mat)
  }
  
  
  distTableFormat <- function(res, src, dst){
    # extract distance table
    mat <- res$distances
    # rounding meters
    mat <- round(mat, 0)
    # col and row names management
    dimnames(mat) <- list(src$id, dst$id)
    return(mat)
  }
  
  coordFormat <- function(res, src, dst){
    sources <- data.frame(matrix(unlist(res$sources$location,
                                        use.names = T),
                                 ncol = 2, byrow = T,
                                 dimnames = list(src$id, c("lon", "lat"))))
    destinations <- data.frame(matrix(unlist(res$destinations$location,
                                             use.names = T),
                                      ncol = 2, byrow = T,
                                      dimnames = list(dst$id, c("lon", "lat"))))
    return(list(sources = sources, destinations = destinations)
    )
  }
  
  tableLoc <- function(loc, gepaf = FALSE,  osrm.server,
                       osrm.profile){
    # Query build
    if (gepaf == TRUE){
      tab <- paste0(osrm.server, "table/v1/", osrm.profile, "/polyline(")
      loc$lat <- as.numeric(as.character(loc$lat))
      loc$lon <- as.numeric(as.character(loc$lon))
      tab <- paste0(tab, googlePolylines::encode(loc[,c("lon","lat")]),")")
    }else{
      tab <- paste0(osrm.server, "table/v1/", osrm.profile, "/")
      tab <- paste0(tab, paste(clean_coord(loc$lon),
                               clean_coord(loc$lat),
                               sep=",",collapse = ";"))
    }
    return(tab)
  }
  
  osrmLimit <- function(nSrc, nDst, nreq){
    e <- simpleError("The public OSRM API does not allow results with a number of durations
higher than 10000. Ask for fewer durations or use your own server and set its
--max-table-size option.")
    e2 <- simpleError("This request is to large for the public OSRM API. Ask for
fewer durations or use your own server and set its --max-table-size option.")
    e3 <- simpleError("This request is to large for the public OSRM API. Ask for
fewer locations or use your own server and set its --max-trip-size option.")
    if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & (nSrc*nDst) > 9998){
      stop(e)
    }
    if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & nreq >= 8000){
      stop(e2)
    }
    if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & nSrc > 99 & nDst==0){
      stop(e3)
    }
    if(getOption("osrm.server") == "https://routing.openstreetmap.de/" & nSrc > 99 & nDst==0){
      stop(e3)
    }
  }
  
  
  
  input_route <- function(x, id, single = TRUE){
    # test various cases (vector, data.frame, with or without id, sf, sp)
    oprj <- NA
    if(single){
      if(is.vector(x)){
        if(length(x) == 2){
          id <- id
          i <- 0
        }else{
          i <- 1
          id <- x[i]
        }
        lon <- clean_coord(x[i+1])
        lat <- clean_coord(x[i+2])
      }
      if(methods::is(x,"Spatial")){
        x <- st_as_sf(x[1,])
      }
      if(is.data.frame(x)){
        if(methods::is(x,"sf")){
          oprj <- sf::st_crs(x)
          x <- sfToDf(x)
          i <- 1
          id <- x[1, i]
        }else{
          if(length(x) == 2){
            i <- 0
            id <- id
          }else{
            i <- 1
            id <- x[1, i]
          }
        }
        lon <- clean_coord(x[1, i+1])
        lat <- clean_coord(x[1, i+2])
      }
      return(list(id = id, lon = lon, lat = lat, oprj = oprj))
    }else{
      if(methods::is(x,"Spatial")){
        x <- st_as_sf(x)
      }
      if(is.data.frame(x)){
        if(methods::is(x,"sf")){
          oprj <- sf::st_crs(x)
          x <- sfToDf(x)
          i <- 1
          id1 <- x[1,1]
          id2 <- x[nrow(x),1]
        }else{
          if(length(x) == 2){
            i <- 0
            id1 <- "src"
            id2 <- "dst"
          }else{
            i <- 1
            id1 <- x[1,1]
            id2 <- x[nrow(x),1]
          }
        }
        lon <- clean_coord(x[, i+1])
        lat <- clean_coord(x[, i+2])
      }
      return(list(id1 = id1, id2 = id2, lon = lon, lat = lat, oprj = oprj))
    }
  }
  osrmTable <- function(loc, src = NULL, dst = NULL, exclude = NULL,
                        gepaf = FALSE, measure="duration",
                        osrm.server = getOption("osrm.server"),
                        osrm.profile = getOption("osrm.profile")){
    if(osrm.server == "https://routing.openstreetmap.de/") {
      osrm.server = paste0(osrm.server, "routed-", osrm.profile, "/")
      osrm.profile = "driving"
    }
    tryCatch({
      # input mgmt
      if (is.null(src)){
        if(methods::is(loc,"Spatial")){
          loc <- st_as_sf(x = loc)
        }
        if(testSf(loc)){
          loc <- sfToDf(x = loc)
        }
        names(loc) <- c("id", "lon", "lat")
        src <- loc
        dst <- loc
        sep <- "?"
        req <- tableLoc(loc = loc, gepaf = gepaf, osrm.server = osrm.server,
                        osrm.profile = osrm.profile)
      }else{
        if(methods::is(src,"Spatial")){
          src <- st_as_sf(x = src)
        }
        if(testSf(src)){
          src <- sfToDf(x = src)
        }
        if(methods::is(dst,"Spatial")){
          dst <- st_as_sf(x = dst)
        }
        if(testSf(dst)){
          dst <- sfToDf(x = dst)
        }
        
        names(src) <- c("id", "lon", "lat")
        names(dst) <- c("id", "lon", "lat")
        
        
        # Build the query
        loc <- rbind(src, dst)
        sep = "&"
        req <- paste(tableLoc(loc = loc, gepaf = gepaf, osrm.server = osrm.server,
                              osrm.profile = osrm.profile),
                     "?sources=",
                     paste(0:(nrow(src)-1), collapse = ";"),
                     "&destinations=",
                     paste(nrow(src):(nrow(loc)-1), collapse = ";"),
                     sep="")
      }
      
      # exclude mngmnt
      if (!is.null(exclude)) {
        exclude_str <- paste0(sep,"exclude=", exclude, sep = "")
        sep="&"
      }else{
        exclude_str <- ""
      }
      
      # annotation mngmnt
      annotations <- paste0(sep, "annotations=", paste0(measure, collapse=','))
      
      # if(getOption("osrm.server") == "http://router.project-osrm.org/"){
      #   annotations <- ""
      # }
      
      # final req
      req <- paste0(req, exclude_str, annotations)
      
      # print(req)
      req <- utils::URLencode(req)
      osrmLimit(nSrc = nrow(src), nDst = nrow(dst), nreq = nchar(req))
      
      # print(req)
      
      # Get the result
      bo=0
      while(bo!=10){
        x = try({
          req_handle <- curl::new_handle(verbose = FALSE)
          curl::handle_setopt(req_handle, useragent = "osrm_R_package")
          resRaw <- curl::curl(req, handle = req_handle)
          res <- jsonlite::fromJSON(resRaw)
        }, silent = TRUE)
        if (class(x)=="try-error") {
          Sys.sleep(1)
          bo <- bo+1
        } else
          break
      }
      
      # Check results
      if(is.null(res$code)){
        e <- simpleError(res$message)
        stop(e)
      }else{
        e <- simpleError(paste0(res$code,"\n",res$message))
        if(res$code != "Ok"){stop(e)}
      }
      
      output <- list()
      if(!is.null(res$durations)){
        # get the duration table
        output$durations <- durTableFormat(res = res, src = src, dst = dst)
      }
      if(!is.null(res$distances)){
        # get the distance table
        output$distances <- distTableFormat(res = res, src = src, dst = dst)
      }
      # get the coordinates
      coords <- coordFormat(res = res, src = src, dst = dst)
      output$sources <- coords$sources
      output$destinations = coords$destinations
      return(output)
    }, error=function(e) {message("The OSRM server returned an error:\n", e)})
    return(NULL)
  }
  
  oprj <- NA
  if (methods::is(loc, "Spatial")) {
    loc <- st_as_sf(loc[1, ])
  }
  if (testSf(loc)) {
    oprj <- st_crs(loc)
    loc <- loc[1, ]
  } else {
    loc <- data.frame(lon = loc[1], lat = loc[2])
    loc <- st_as_sf(loc, coords = c("lon", "lat"), crs = 4326)
  }
  loc <- st_transform(loc, 3857)
  row.names(loc) <- "0"
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  if (osrm.profile %in% c("foot", "walk")) {
    speed = 10 * 1000 / 60
  }
  if (osrm.profile == "bike") {
    speed = 20 * 1000 / 60
  }
  if (osrm.profile %in% c("driving", "car")) {
    speed = 130 * 1000 / 60
  }
  dmax <- tmax * speed
  sgrid <- rgrid(loc = loc, dmax = dmax, res = res)
  if (osrm.server != "https://routing.openstreetmap.de/") {
    sleeptime <- 0
    deco <- 300
  } else {
    sleeptime <- 1
    deco <- 100
    osrm.server = paste0(osrm.server, "routed-", osrm.profile, "/")
    osrm.profile = "driving"
  }
  lsgr <- nrow(sgrid)
  f500 <- lsgr %/% deco
  r500 <- lsgr %% deco
  listDur <- list()
  listDest <- list()
  if (f500 > 0) {
    for (i in 1 : f500) {
      st <- (i - 1) * deco + 1
      en <- i * deco
      dmat <- osrmTable(src = loc, dst = sgrid[st : en, ], exclude = exclude, osrm.server = osrm.server, osrm.profile = osrm.profile)
      durations <- dmat$durations
      listDur[[i]] <- dmat$durations
      listDest[[i]] <- dmat$destinations
      Sys.sleep(sleeptime)
    }
    if (r500 > 0) {
      dmat <- osrmTable(src = loc, dst = sgrid[(en + 1) : (en + r500), ], exclude = exclude, osrm.server = osrm.server, osrm.profile = osrm.profile)
      listDur[[i + 1]] <- dmat$durations
      listDest[[i + 1]] <- dmat$destinations
    }
  } else {
    dmat <- osrmTable(src = loc, dst = sgrid, exclude = exclude, osrm.server = osrm.server, osrm.profile = osrm.profile)
    listDur[[1]] <- dmat$durations
    listDest[[1]] <- dmat$destinations
  }
  durations <- do.call(c, listDur)
  destinations <- do.call(rbind, listDest)
  rpt <- st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)
  rpt <- st_transform(rpt, st_crs(loc))
  rpt$durations <- durations
  b <- as.numeric(st_distance(sgrid[1, ], sgrid[2, ]) / 2)
  xx <- st_make_grid(x = st_buffer(st_as_sfc(st_bbox(sgrid)), b), n = c(res, res))
  inter <- st_intersects(xx, rpt)
  sgrid$durations <- unlist(lapply(inter, function(x) mean(rpt[["durations"]][x], na.rm = TRUE)))
  sgrid[is.nan(sgrid$durations), "durations"] <- tmax + 1
  sgrid[sgrid$durations > tmax, "durations"] <- tmax + 1
  if (min(sgrid$durations) > tmax) {
    e <- "Use lower values for \'breaks\' or increase \'res\'"
    stop(e, call. = FALSE)
  }
  iso <- isopoly(x = sgrid, breaks = breaks, var = "durations")
  if (!is.na(oprj)) {
    iso <- st_transform(x = iso, oprj)
  } else {
    iso <- st_transform(x = iso, 4326)
  }
  if (returnclass == "sp") {
    iso <- methods::as(iso, "Spatial")
  }
  return(iso)
}