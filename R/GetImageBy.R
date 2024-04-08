#' @title Get image from the archive (vectorization ready)
#' @description These functions retrieve the image for the area of interest using the parameters provided.
#'     They are simple wrappers around the 'GetImage' function with arguments organized in a way
#'     that facilitates calling the function in a vectorized manner (using 'lapply' or similar function)
#'     and thus potentially also the parallelization.
#' @return \code{SpatRaster} object (from the package \code{terra}) of the requested image (if \code{file} is \code{NULL}),
#'     or the (invisible) name of the file created.
#' @details If \code{aoi} argument is provided, the result is returned in the same coordinate reference system.
#' @examples
#' \dontrun{
#' dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
#' aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#' cloudless_images <- SearchCatalog(aoi = aoi, from = "2023-01-01", to = "2023-12-31",
#'                     collection = "sentinel-2-l2a", with_geometry = TRUE,
#'                     filter = "eo:cloud_cover < 0.8", client = OAuthClient)
#' script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
#' days <- rev(cloudless_images$acquisitionDate)
#' lstRast <- lapply(days, GetImageByTimerange, aoi = aoi, collection = "sentinel-2-l2a",
#'     script = script_file, file = NULL, format = "image/tiff", mosaicking_order = "mostRecent",
#'     resolution = 10, buffer = 0, mask = TRUE, client = OAuthClient,
#'     url = getOption("CDSE.process_url"))
#' par(mfrow = c(3, 4))
#' sapply(seq_along(days), FUN = function(i) {
#'      ras <- lstRast[[i]]
#'      day <- days[i]
#'      ras[ras < 0] <- 0
#'      terra::plot(ras, main = paste("Central Park NDVI on", day), range = c(0, 1),
#'             cex.main = 0.7, pax = list(cex.axis = 0.5), plg = list(cex = 0.5),
#'             col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))
#'      })
#' }
#' @seealso
#'  \code{\link[CDSE]{GetImage}}
#' @rdname GetImageBy...
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Process.html}
#' @importFrom sf st_transform st_geometry st_bbox st_buffer st_coordinates st_centroid
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform
#' @importFrom terra rast crs project mask writeRaster

#' @name GetImageBy...
#' @param time_range scalar or vector (Date or character that can be converted to date) defining the time interval.
#' @param collection character indicating which collection to search.
#'     Must be one of the collections returned by \code{GetCollections}.
#' @param script a length one character string containing the evaluation script or the name of the file containing the script.
#' @param mosaicking_order character indicating the order in which tiles are overlapped from which the output result is mosaicked.
#'     Must be one of "mostRecent", "leastRecent", or "leastCC". Default: "mostRecent"
#' @param file name of the file to save the image. If NULL, a \code{SpatRaster} object is returned. Default: NULL
#' @param format character indicating the output file format.
#'     Must be one of "image/tiff", "image/png", or "image/jpeg". Default: "image/tiff"
#' @param pixels integer scalar or length-two vector indicating the request image width and height.
#'     Values must be integers between 1 and 2500.
#' @param resolution numeric scalar or length-two vector indicating the spatial resolution of the request image
#'     in horizontal and vertical direction (in meters).
#'
#' Only one of the arguments "pixels" or "resolution" must be set at the same time.
#' If the argument "pixels" or "resolution" is scalar, the same value is used for horizontal and vertical direction (width and height).
#' @param buffer numeric, width of the buffer to retrieve the image of enlarged area. Default: 0
#' @param mask logical indicating if the image should contain only pixels within Area of Interest. Default: FALSE
#' @param client OAuth client object to use for authentication.
#' @param token OAuth token character string to use for authentication.
#'
#' Exactly one of either \code{client} or \code{token} must be specified. It is recommended to use \code{client}.
#' @param url character indicating the process endpoint. Default: Copernicus Data Space Ecosystem process endpoint

#' @details \code{GetImageByTimerange} is arranged for vectorization on time_range (time_range is the first argument).
#' @export
GetImageByTimerange <- function(time_range, aoi, bbox, collection, script, file = NULL,
                                       format = c("image/tiff", "image/png", "image/jpeg"),
                                       mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                                       pixels, resolution, buffer = 0, mask = FALSE,
                                       client, token, url = getOption("CDSE.process_url")) {
    return(GetImage(aoi = aoi, bbox = bbox, time_range = time_range, collection = collection, script = script,
                    file = file, format = format, mosaicking_order = mosaicking_order, pixels = pixels,
                    resolution = resolution, buffer = buffer, mask = mask,
                    client = client, token = token, url = url))
}

#' @name GetImageBy...
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
#' @details \code{GetImageByAOI} is arranged for vectorization on aoi (aoi is the first argument).
#' @export
GetImageByAOI <- function(aoi, time_range, collection, script, file = NULL,
                            format = c("image/tiff", "image/png", "image/jpeg"),
                            mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                            pixels, resolution, buffer = 0, mask = FALSE,
                            client, token, url = getOption("CDSE.process_url")) {
    return(GetImage(aoi = aoi, time_range = time_range, collection = collection, script = script,
                    file = file, format = format, mosaicking_order = mosaicking_order, pixels = pixels,
                    resolution = resolution, buffer = buffer, mask = mask,
                    client = client, token = token, url = url))
}

#' @name GetImageBy...
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#'     Coordinates need to be in longitude, latitude.
#'
#' Only one of either \code{aoi} or \code{bbox} may be specified.
#' @details \code{GetImageByBbox} is arranged for vectorization on bbox (bbox is the first argument).
#' @export
GetImageByBbox <- function(bbox, time_range, collection, script, file = NULL,
                                 format = c("image/tiff", "image/png", "image/jpeg"),
                                 mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                                 pixels, resolution, buffer = 0, mask = FALSE,
                                 client, token, url = getOption("CDSE.process_url")) {
    return(GetImage(bbox = bbox, time_range = time_range, collection = collection, script = script,
                    file = file, format = format, mosaicking_order = mosaicking_order, pixels = pixels,
                    resolution = resolution, buffer = buffer, mask = mask,
                    client = client, token = token, url = url))
}
