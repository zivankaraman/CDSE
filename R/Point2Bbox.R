#' @title Create bounding box around a point
#' @description Creates the bounding box (numeric vector of length four) around the input point(s).
#' @param x an \code{sf}, \code{sfc}, or \code{SpatialPoints*} object, a numeric indicating the longitude/easting of the point(s),
#'     or any input accepted by \code{xy.coords}
#' @param y numeric, the latitude/northing of the point(s). Default: NULL
#' @param size numeric indicating the size (in meters) of the bounding box to create
#' @param crs coordinate reference system of the input (and the output): object of class \code{crs},
#'     or input string for \code{st_crs}. Default: 4326 (WGS 84)
#' @return A bounding box (numeric vector of length four), or a list of bounding boxes if the input is not scalar.
#' @details The function assumes that the \code{crs} units are either degrees or meters, a warning is issued if not,
#'     and the result will probably be incorrect.
#' @examples
#' \dontrun{
#' Point2Bbox(x = -73.96557, y = 40.78246, size = 1000, crs = 4326)
#' }
#' @seealso
#'  \code{\link[grDevices]{xy.coords}}, \code{\link[sf]{st_crs}}
#' @rdname Point2Bbox
#' @export
#' @importFrom sf st_as_sf st_geometry_type st_coordinates st_crs
Point2Bbox <- function(x, y = NULL, size, crs = 4326) {
    allowed_class <- c("sf", "sfc", "SpatialPoints")
    ndx <- inherits(x, allowed_class, which = TRUE)
    if (sum(ndx) == 0L) {
        coords <- grDevices::xy.coords(x, y)
        xcoord <- coords$x
        ycoord <- coords$y
    } else {
        type <- allowed_class[which(ndx > 0L)]
        if (type == "SpatialPoints") {
            x <- sf::st_as_sf(x)
        } else {
            if (any(sf::st_geometry_type(x) != "POINT")) {
                stop("geometry must be of type 'POINT'")
            }
        }
        coords <- sf::st_coordinates(x)
        crs <- sf::st_crs(x)
        xcoord <- coords[, "X"]
        ycoord <- coords[, "Y"]

    }
    pts <- sf::st_as_sf(data.frame(xcoord, ycoord), coords = 1:2, crs = crs)

    crs_units <- sf::st_crs(crs, parameters = TRUE)$units_gdal
    if (is.na(match(crs_units, c("degree", "matre")))) {
        msg <- sprintf("The crs unit is expected to be either 'degree' or 'metre', not '%s', results might be incorrect", crs_units)
        warning(msg)
    }

    # size
    size <- rep_len(size, 2)
    if (crs_units == "degree") {
        # convert to degrees
        # size <-  size / matrix(CDSE:::DegLength(ycoord), ncol = 2)
        deg_size <- DegLength(ycoord)
        if (is.null(dim(deg_size))) {
            deg_size <- matrix(deg_size, ncol = 2)
        }
        size <-  size / deg_size
    }

    delta_x <- size[, 1, drop = TRUE] / 2.0
    delta_y <- size[, 2, drop = TRUE] / 2.0

    mat <- cbind(xcoord - delta_x, ycoord - delta_y, xcoord + delta_x, ycoord + delta_y)
    dimnames(mat) <- list(NULL, c("xmin", "ymin", "xmax", "ymax"))
    out <- lapply(1:nrow(mat), function(i) mat[i, , drop = TRUE])
    if (length(out) == 1L) {
        out <- out[[1]]
    }
    return(out)
}
