between <- function(x, left, right) {
    x >= left & x <= right
}


LastError <- function() {
    # Sentinel Hub API uses boom - HTTP-friendly error objects
    # https://hapi.dev/module/boom/
    resp <- httr2::last_response()
    err <- as.data.frame(httr2::resp_body_json(resp)[[1]])
    msg <- c(" \n", paste0(capture.output(err), "\n"))
}

SafeNull <- function(x) {
    ifelse(is.null(x), NA, x)
}

CheckLengthIs2 <- function(x) {
    # if one value only provided, replicate it twice (use it for both X and Y)
    if (length(x) == 1L) {
        x <- rep(x, 2)
    }
    if (length(x) != 2L) {
        msg <- sprintf("%s must be a scalar or a vector of length 2", deparse(substitute(x)))
        stop(msg)
    }
    return(x)
}

CheckBbox <- function(bbox) {
    if (!is.numeric(bbox) | (length(bbox) != 4L)) {
            stop("Invalid value of bbox argument, must be a numeric vector of length 4.")
    }
    mat <- matrix(bbox, ncol = 2, byrow = TRUE)
    if (any(mat[, 1] > 90 | mat[, 1] < -90) |
        any(mat[, 2] > 180 | mat[, 2] < -180) |
        any(mat[1, ] > mat[2, ])) {
        stop("Invalid values in bbox, must be longitude/latitude of lower-left/upper-right corner.")
    }
    return()
}

DegLength <- function(latitude) {
    # Computes the length (in meters) of one degree of longitude (X) and one degree of latitude (Y)
    #     at a given "latitude" using the WGS 84 ellipsoid parameters.
    #
    # WGS 84 ellipsoid parameters
    # Equatorial radius "a" (called semi-major axis)
    a <- 6378137.0
    # Polar radius "b" (called semi-minor axis)
    b <- 6356752.3142
    # eccentricity "e" of the ellipsoid is related to the major and minor axes (the equatorial and polar radii respectively)
    e2 <- (a^2 - b^2) / a^2     # e-square

    # convert degrees to radians
    phi <- (latitude * pi) / 180.0

    # Length of a degree of longitude
    # https://en.wikipedia.org/wiki/Longitude#Length_of_a_degree_of_longitude
    degX <- (pi * a * cos(phi)) / (180.0 * sqrt(1 - e2 * sin(phi)^2))
    # Length of a degree of latitude
    # https://en.wikipedia.org/wiki/Latitude#Length_of_a_degree_of_latitude
    degY <- (pi * a * (1 - e2)) / (180.0 * (1 - e2 * sin(phi)^2)^(3/2))
    if (length(latitude) > 1L) {
        out <- data.frame(X = degX, Y = degY)
    } else {
        out <- c(degX, degY)
        names(out) <- c("X", "Y")
    }
    return(out)
}

MakeTimeRange <- function(period) {
    if (any(c(inherits(period, "character"), inherits(period, "Date")))) {
        from <- as.Date(min(period))
        to <- as.Date(max(period))
    }
    if (any(c(inherits(period, "integer"), inherits(period, "numeric")))) {
        from <- min(period)
        to <- max(period)
        from <- as.Date(as.character(from), format = "%Y%m%d")
        to <- as.Date(as.character(to), format = "%Y%m%d")
    }
    return(list(from = sprintf("%sT00:00:00.000Z", from), to = sprintf("%sT23:59:59.000Z", to)))
}


Flatten <- function(x) {
    allowed_class <- c("Raster", "sf", "sfc", "Spatial", "SpatRaster", "SpatVector")
    ndx <- inherits(x, allowed_class, which = TRUE)
    if (sum(ndx) == 0L) {
        stop("x must be an Raster, sf, sfc, Spatial*, SpatRaster or SpatVector object")
    } else {
        type <- allowed_class[which(ndx > 0L)]
    }
    # get bounding box as geometry of type points
    boxpts <- sf::st_as_sfc(sf::st_bbox(x))
    # convert to geographic coordinates (longitude / latitude)
    boxLL <- matrix(sf::st_bbox(sf::st_transform(boxpts, crs = 4326)), ncol = 2)
    # find the center
    llc <- apply(boxLL, 1, mean)
    # construct the projection string with a minimal angle to avoid errors
    angle <- 0.00001
    prj <- paste0("+proj=omerc +lat_0=", llc[2], " +lonc=", llc[1], " +alpha=", angle,
                  " +gamma=0.0 +k=1.000000 +x_0=0.000 +y_0=0.000 +ellps=WGS84 +units=m ")
    # transform to the new planar projection
    out <- switch(type,
                  Raster = raster::projectRaster(x, crs = prj),
                  sf = sf::st_transform(x, crs = sf::st_crs(prj)$wkt),
                  sfc = sf::st_transform(x, crs = sf::st_crs(prj)$wkt),
                  Spatial = sf::as_Spatial(sf::st_transform(sf::st_as_sf(x), crs = sf::st_crs(prj)$wkt)),
                  SpatRaster = terra::project(x, sf::st_crs(prj)$wkt),
                  SpatVector = terra::project(x, sf::st_crs(prj)$wkt),
                  NULL)
    return(out)
}


PolyFromBbox <- function(x) {
    # creates sf geometry from bbox vector
    vec <- c(x[1], x[2],
             x[3], x[2],
             x[3], x[4],
             x[1], x[4],
             x[1], x[2])
    mat <- matrix(vec, ncol = 2, byrow = TRUE)
    sf::st_as_sfc(list(sf::st_polygon(list(mat))), crs = 4326)
}


# Bufferize <- function(bounds, buffer, join = "MITRE") {
#     bounds |> Flatten() |>
#         sf::st_buffer(dist = buffer, joinStyle = join, mitreLimit = 999999) |>
#         sf::st_transform(crs = sf::st_crs(bounds))
# }
