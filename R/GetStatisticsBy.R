#' @title Get statistical values (vectorization ready)
#' @description These functions retrieve simple statistics for the area of interest calculated based on satellite imagery
#'     without having to download images.
#'     They are simple wrappers around the 'GetStatistics' function with arguments organized in a way
#'     that facilitates calling the function in a vectorized manner (using 'lapply' or similar function)
#'     and thus potentially also the parallelization.

#' @return \code{data.frame} or \code{list} with statistical values.
#' @details The values are aggregated over the period (number of days) given by the \code{aggregation_period} argument.
#'    The default value 1 provides daily statistics. The statistics are returned only for the days when the data is
#'    available (the days of the satellite overpass).
#'
#'    The scripts used for the Statistical API have some additional requirements: the \code{evaluatePixel()}
#'        function must, in addition to other output, always also return dataMask output.
#'        This output defines which pixels are excluded from calculations.
#'    For more information please visit the online documentation
#'    \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Statistical.html}.
#'
#'    If percentiles requested are 25, 50, and 75, the columns are renamed 'q1', 'median', and 'q3'.
#' @examples
#' \dontrun{
#' dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
#' aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#' script_file <- "inst/scripts/NDVI_dataMask_float32.js"
#' seasons <- SeasonalTimerange(from = "2020-06-01", to = "2023-08-31")
#' lst_stats <- lapply(seasons, GetStatisticsByTimerange, aoi = aoi, collection = "sentinel-2-l2a",
#'    script = script_file, mosaicking_order = "leastCC", resolution = 100,
#'    aggregation_period = 7L, client = OAuthClient)
#' weekly_stats <- do.call(rbind, lst_stats)
#' weekly_stats <- weekly_stats[rev(order(weekly_stats$from)), ]
#' row.names(weekly_stats) <- NULL
#' head(weekly_stats)
#' }
#' @seealso \code{\link[CDSE]{GetCollections}}, \code{\link[CDSE]{SearchCatalog}}, \code{\link[CDSE]{GetStatistics}}
#' @rdname GetStatisticsBy...
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Statistical.html}
#' @importFrom sf st_transform st_geometry st_bbox st_buffer st_coordinates st_centroid
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform

#' @name GetStatisticsBy...
#' @param time_range scalar or vector (Date or character that can be converted to date) defining the time interval.
#' @param collection character indicating which collection to search.
#'     Must be one of the collections returned by \code{GetCollections}.
#' @param script a length one character string containing the evaluation script or the name of the file containing the script.
#' @param mosaicking_order character indicating the order in which tiles are overlapped from which the output result is mosaicked.
#'     Must be one of "mostRecent", "leastRecent", or "leastCC". Default: "mostRecent"
#' @param pixels integer scalar or length-two vector indicating the request image width and height.
#'     Values must be integers between 1 and 2500.
#' @param resolution numeric scalar or length-two vector indicating the spatial resolution of the request image
#'     in horizontal and vertical direction (in meters).
#'
#'     Only one of the arguments "pixels" or "resolution" must be set at the same time.
#'     If the argument "pixels" or "resolution" is scalar, the same value is used for horizontal and vertical direction (width and height).
#' @param buffer numeric, width of the buffer to retrieve the image of enlarged area. Default: 0
#' @param percentiles numeric vector indicating which percentile values should be computed.
#'     Default: NULL, don't compute any percentiles.
#' @param aggregation_period the length of the aggregation period in days. Default: 1
#' @param as_data_frame logical indicating if the result should be returned as data frame. Default: TRUE
#' @param client OAuth client object to use for authentication.
#' @param token OAuth token character string to use for authentication.
#'
#'     Exactly one of either \code{client} or \code{token} must be specified. It is recommended to use \code{client}.
#' @param url character indicating the process endpoint. Default: Copernicus Data Space Ecosystem process endpoint
#' @details \code{GetStatisticsByTimerange} is arranged for vectorization on time_range (time_range is the first argument).
#' @export
GetStatisticsByTimerange <- function(time_range, aoi, bbox, collection, script,
                               mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                               pixels, resolution, buffer = 0, percentiles = NULL, aggregation_period = 1L, as_data_frame = TRUE,
                               client, token, url = getOption("CDSE.statistical_url")) {
    return(GetStatistics(time_range = time_range, aoi = aoi, bbox = bbox, collection = collection, script = script,
                  mosaicking_order = mosaicking_order, pixels = pixels, resolution = resolution, buffer = buffer,
                  percentiles = percentiles, aggregation_period = aggregation_period, as_data_frame = as_data_frame,
                  client = client, token = token, url = url))
}


#' @name GetStatisticsBy...
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
#' @details \code{GetStatisticsByAOI} is arranged for vectorization on aoi (aoi is the first argument).
#' @export
GetStatisticsByAOI <- function(aoi, time_range, collection, script,
                               mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                               pixels, resolution, buffer = 0, percentiles = NULL, aggregation_period = 1L, as_data_frame = TRUE,
                               client, token, url = getOption("CDSE.statistical_url")) {
    return(GetStatistics(aoi = aoi, time_range = time_range, collection = collection, script = script,
                  mosaicking_order = mosaicking_order, pixels = pixels, resolution = resolution, buffer = buffer,
                  percentiles = percentiles, aggregation_period = aggregation_period, as_data_frame = as_data_frame,
                  client = client, token = token, url = url))
}

#' @name GetStatisticsBy...
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#'     Coordinates need to be in longitude, latitude.
#'
#' Only one of either \code{aoi} or \code{bbox} may be specified.
#' @details \code{GetStatisticsByBbox} is arranged for vectorization on bbox (bbox is the first argument).
#' @export
GetStatisticsByBbox <- function(bbox, time_range, collection, script,
                               mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                               pixels, resolution, buffer = 0, percentiles = NULL, aggregation_period = 1L, as_data_frame = TRUE,
                               client, token, url = getOption("CDSE.statistical_url")) {
    return(GetStatistics(bbox = bbox, time_range = time_range, collection = collection, script = script,
                  mosaicking_order = mosaicking_order, pixels = pixels, resolution = resolution, buffer = buffer,
                  percentiles = percentiles, aggregation_period = aggregation_period, as_data_frame = as_data_frame,
                  client = client, token = token, url = url))
}
