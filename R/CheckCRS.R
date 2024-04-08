CheckCRS <- function(x) {
    match_crs <- function(x, table, nomatch = NA_integer_) {
        if (any(c(class(x), sapply(table, class)) != "crs")) {
            stop("input not of class 'crs'")
        }
        idx <- which(sapply(table, FUN = function(x1, x2) x1 == x2, x))
        if (length(idx) == 0L) {
            out <- nomatch
        } else {
            out <- idx[1]
        }
        return(out)
    }
    allowedCRS <- c(4326, 3857, 32601:32660, 32701:32760, 2154, 2180, 2193, 3003, 3004, 3006,
                    3031, 3035, 3346, 3413, 3416, 3765, 3794, 3844, 3912, 3995, 4026, 5514, 28992)
    lstCRS <- lapply(allowedCRS, sf::st_crs)
    i <- match_crs(x, lstCRS)
    if (is.na(i)) {
        # stop("This CRS is not supported by the API, please refer to 'https://docs.sentinel-hub.com/api/latest/api/process/crs/'")
        stop("This CRS is not supported by the API, please refer to 'https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Process/Crs.html'")
    } else {
        return(allowedCRS[i])
    }
}

# crs <- sf::st_crs(bounds, parameters = TRUE)
# crs$units
# crs$epsg

# x = st_crs(3032)
# CheckCRS(st_crs(3032))
#
#
#
# x <- st_crs(32623)
# x <- st_crs(4326)
# sapply(lst, FUN = function(x, y) x == y, x)
# table = lst
#
# match_crs <- function(x, table, nomatch = NA_integer_) {
#     if (any(c(class(x), sapply(table, class)) != "crs")) {
#         stop()
#     }
#     idx <- which(sapply(table, FUN = function(x1, x2) x1 == x2, x))
#     if (length(idx) == 0L) {
#         out <- nomatch
#     } else {
#         out <- idx[1]
#     }
#     return(out)
# }
#
# match_crs(x, lst)
