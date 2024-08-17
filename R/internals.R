between <- function(x, left, right) {
    x >= left & x <= right
}

#' @importFrom httr2 last_response resp_body_json
#' @noRd
LastError <- function() {
    # Sentinel Hub API uses boom - HTTP-friendly error objects
    # https://hapi.dev/module/boom/
    resp <- httr2::last_response()
    err <- as.data.frame(httr2::resp_body_json(resp)[[1]])
    msg <- paste0("\n", paste(paste(names(err), err, sep = ": "), collapse = "\n"))
    return(msg)
}

LastResponse <- function() {
    # Sentinel Hub API uses boom - HTTP-friendly error objects
    # https://hapi.dev/module/boom/
    return(httr2::last_response())
}

SafeNull <- function(x) {
    return(ifelse(is.null(x), NA, x))
}

CheckCredential <- function(x) {
    x <- as.character(x)
    if (is.na(x) || is.null(x) || (length(x) == 0L)) x <- ""
    return(x)
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

CheckMosaicking <- function(x) {
    idx <- pmatch(tolower(x), tolower(c("mostRecent", "leastRecent", "leastCC")))
    if (is.na(idx)) {
        msg <- stop("Invalid value of 'mosaicking_order' argument, must be one of 'mostRecent', 'leastRecent', or 'leastCC'")
        stop(msg)
    } else {
        out <- c("mostRecent", "leastRecent", "leastCC")[idx]
        if (out != x) {
            msg <- sprintf("The correct spelling for '%s' is '%s', accpeted anyway", x, out)
            warning(msg)
        }
        return(out)
    }
}

CheckFormat <- function(x) {
    idx <- pmatch(tolower(x), tolower(c("image/tiff", "image/png", "image/jpeg", "image/jpg")))
    if (is.na(idx)) {
        # try with extension only, because we are nice :-)
        idx <- pmatch(tolower(x), tolower(c("tiff", "png", "jpeg", "jpg")))
    }
    if (is.na(idx)) {
        msg <- stop("Invalid value of 'format' argument, must be one of 'image/tiff', 'image/png', or 'image/jpeg'")
        stop(msg)
    } else {
        if (idx == 4L) idx <- 3L # fix for jpg
        out <- c("image/tiff", "image/png", "image/jpeg")[idx]
        if (out != x) {
            msg <- sprintf("The correct spelling for '%s' is '%s', accpeted anyway", x, out)
            warning(msg)
        }
        return(out)
    }
}

CheckAOI <- function(aoi) {
    out <- NULL
    if (inherits(aoi, "sf")) {
        if (nrow(aoi) == 0L) {
            stop("empty 'aoi' value")
        }
        if (nrow(aoi) > 1L) {
            out <- sf::st_union(aoi)
        } else {
            out <- aoi
        }
    }
    if (inherits(aoi, "sfc")) {
        if (sf::st_is_empty(aoi)) {
            stop("empty 'aoi' value")
        }
        if (length(aoi) > 1L) {
            out <- sf::st_union(aoi)
        } else {
            out <- aoi
        }
        out <- sf::st_as_sf(out)
    }
    if (is.null(out)) {
        stop("invalid 'aoi' value")
    }
    return(out)
}

CheckBbox <- function(bbox) {
    # try converting to numeric, just in case it is not
    bbox <- try(as.numeric(bbox), silent = TRUE)
    if (inherits(bbox, "try-error")) {
        stop("Invalid value of 'bbox' argument, must be a numeric vector of length 4.")
    }
    # check class and length
    if (length(class(bbox)) > 1L || !inherits(bbox, "numeric") || (length(bbox) != 4L)) {
            stop("Invalid value of 'bbox' argument, must be a numeric vector of length 4.")
    }
    # convert to matrix for easier range checks
    mat <- matrix(bbox, ncol = 2, byrow = TRUE)
    if (any(mat[, 1] > 90 | mat[, 1] < -90) ||
        any(mat[, 2] > 180 | mat[, 2] < -180) ||
        any(mat[1, ] > mat[2, ])) {
        stop("Invalid values in 'bbox', must be longitude/latitude of lower-left/upper-right corner.")
    }
    return(bbox)
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

MakeTimeRange <- function(period, format = TRUE) {
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
    if (isTRUE(format)) {
        out <- list(from = sprintf("%sT00:00:00.000Z", from), to = sprintf("%sT00:00:00.000Z", to + 1))
    } else {
        out <- list(from = from, to = to)
    }
    return(out)
}

#' @importFrom sf st_as_sfc st_polygon
#' @noRd
PolyFromBbox <- function(x, crs = 4326) {
    # creates sf geometry from bbox vector
    vec <- c(x[1], x[2],
             x[3], x[2],
             x[3], x[4],
             x[1], x[4],
             x[1], x[2])
    mat <- matrix(vec, ncol = 2, byrow = TRUE)
    out <- sf::st_as_sfc(list(sf::st_polygon(list(mat))), crs = crs)
    return(out)
}

grepOptions <- function(pattern) {
    opt <- options()
    return(opt[grep(pattern, names(opt))])
}
