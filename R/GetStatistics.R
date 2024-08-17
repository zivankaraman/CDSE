#' @title Get statistical values
#' @description Retrieves the simple statistics for the area of interest calculated based on satellite imagery
#'     without having to download images.
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#'     Coordinates need to be in longitude, latitude.
#'
#'     Only one of either \code{aoi} or \code{bbox} may be specified.
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
#' @param lastIntervalBehavior = c("SKIP", "SHORTEN", "EXTEND")
#' @param as_data_frame logical indicating if the result should be returned as data frame. Default: TRUE
#' @param client OAuth client object to use for authentication.
#' @param token OAuth token character string to use for authentication.
#'
#'     Exactly one of either \code{client} or \code{token} must be specified. It is recommended to use \code{client}.
#' @param url character indicating the process endpoint. Default: Copernicus Data Space Ecosystem process endpoint
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
#' script_file <- system.file("scripts", "NDVI_CLOUDS_STAT.js", package = "CDSE")
#' daily_stats <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
#'   collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC",
#'   resolution = 100, aggregation_period = 1, client = OAuthClient)
#' weekly_stats <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
#'   collection = "sentinel-2-l2a", script = script_file,mosaicking_order = "leastCC",
#'   resolution = 100, aggregation_period = 7, client = OAuthClient)
#' weekly_stats_extended <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
#'   collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC",
#'   resolution = 100, aggregation_period = 7, lastIntervalBehavior = "EXTEND", client = OAuthClient)
#' }
#' @seealso
#'  \code{\link[CDSE]{GetCollections}}, \code{\link[CDSE]{SearchCatalog}}
#' @rdname GetStatistics
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Statistical.html}
#' @importFrom sf st_transform st_geometry st_bbox st_buffer st_coordinates st_centroid
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform
GetStatistics <- function(aoi, bbox, time_range, collection, script,
                          mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                          pixels, resolution, buffer = 0, percentiles = NULL, aggregation_period = 1L,
                          lastIntervalBehavior = c("SKIP", "SHORTEN", "EXTEND"), as_data_frame = TRUE,
                          client, token, url = getOption("CDSE.statistical_url")) {
    # Only one of either aoi or bbox may be specified.
    if (!missing(aoi) & !missing(bbox)) {
        stop("Only one of either aoi or bbox may be specified.")
    }
    if (missing(aoi) & missing(bbox)) {
        stop("Either aoi or bbox must be specified.")
    }
    # Check bbox is valid (and transform as.numeric if needed)
    if (!missing(bbox)) {
        bbox <- CheckBbox(bbox)
    }
    # authentication
    if (missing(client) & missing(token)) {
        stop("Either client or token must be specified.")
    }
    # Only one of either pixels or resolution may be specified.
    if (!missing(pixels) & !missing(resolution)) {
        stop("Only one of either pixels or resolution may be specified.")
    }
    if (missing(pixels) & missing(resolution)) {
        stop("Either pixels or resolution must be specified.")
    }
    if (!missing(pixels)) {
        pixels <- CheckLengthIs2(pixels)
    }
    if (!missing(resolution)) {
        resolution <- CheckLengthIs2(resolution)
    }
    # check or get the first (default) value if not specified
    mosaicking_order <- CheckMosaicking(mosaicking_order[1])
    lastIntervalBehavior <- lastIntervalBehavior[1]
    if (missing(aoi)) { # query by bbox
        # make bounds from bbox
        bounds <- PolyFromBbox(bbox)
    } else { # query by aoi / intersects
        aoi <- CheckAOI(aoi)
        # convert to WGS84 first to get longitude/latitude coordinates
        bounds <- sf::st_transform(sf::st_geometry(aoi), crs = 4326)
        # bounding box
        bbox <- as.numeric(sf::st_bbox(bounds))
    }
    # add buffer if required
    if (buffer > 0) {
        bounds <- sf::st_buffer(bounds, dist = buffer, joinStyle = "MITRE", mitreLimit = 999999)
        bbox <- as.numeric(sf::st_bbox(bounds))
    }
    data <- list(
        list(
            dataFilter = list(mosaickingOrder = mosaicking_order[1]),
            type = collection)
    )
    # build input part of the request
    if (missing(aoi)) { # query by bbox
        input <- list(bounds = list(bbox = bbox,
                                    properties = list(crs = "http://www.opengis.net/def/crs/OGC/1.3/CRS84")),
                      data = data)
    } else { # query by aoi
        geom <- geojsonsf::sfc_geojson(sf::st_geometry(bounds))
        input <- list(bounds = list(bbox = bbox, geometry = jsonlite::fromJSON(geom),
                                    properties = list(crs = "http://www.opengis.net/def/crs/OGC/1.3/CRS84")),
                      data = data)
    }
    # aggregation
    # get the time range
    period <- MakeTimeRange(time_range)
    aggregationInterval <- list(of = sprintf("P%dD", aggregation_period), lastIntervalBehavior = lastIntervalBehavior)
    # build output part of the request
    if (missing(resolution)) { # use width and height provided
        output <- list(width = pixels[1], height = pixels[2])
    } else {
        # compute the resolution at the latitude of the centroid
        lat <- sf::st_coordinates(sf::st_centroid(bounds))[1, "Y"]
        res <- as.numeric(resolution / DegLength(lat))
        output <- list(resx = res[1], resy = res[2])
    }
    # read the evalscript from file if needed
    if (file.exists(script)) {
        script <- paste(readLines(script), collapse = "\n")
    }
    aggregation <- list(timeRange = period, aggregationInterval = aggregationInterval, evalscript = script)
    aggregation <- c(aggregation, output)
    # make the request body
    if (is.null(percentiles)) {
        bdy <- list(input = input, aggregation = aggregation)
    } else {
        calculations <- list(default = list(statistics = list(default = list(percentiles = list(k = percentiles)))))
        # make the request body
        bdy <- list(input = input, aggregation = aggregation, calculations = calculations)
    }
    # build the request
    req <- httr2::request(url)
    req <- httr2::req_headers(req, "Content-Type" = "application/json", Accept = "*/*")
    req <- httr2::req_body_json(req, bdy)
    # select the appropriate authentication method
    if (missing(client)) {
        req <- httr2::req_auth_bearer_token(req, token = as.character(token))
    } else {
        req <- httr2::req_oauth_client_credentials(req, client = client)
    }
    # run the request
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
        if (length(grep("SSL peer certificate", resp[1])) == 1L) {
            req <- httr2::req_options(req, ssl_verifyhost = 0L, ssl_verifypeer = 0L)
            resp <- httr2::req_perform(req)
            warning("Host SSL certificate seems to have an issue (probably expired)")
        } else {
            stop(LastError())
        }
    }
    # process the response
    cnt <- httr2::resp_body_json(resp)
    lst <- cnt$data
    if (isTRUE(as_data_frame)) {
        GetStatsOuput <- function(x, aggregation_period) {
            GetStatsParams <- function(band, y, output) {
                df <- data.frame(output = output, band = band, y$bands[[band]]$stats, stringsAsFactors = FALSE)
                cNames <- names(df)
                colStart <- c("output", "band", "min")
                colEnd <- c("max", "stDev", "sampleCount", "noDataCount")
                iperc <- grep("percentiles", cNames)
                perc <- as.numeric(sub("percentiles.", "", cNames[iperc]))
                df[, iperc] <- df[, iperc[order(perc)]]
                names(df)[iperc] <- sub("percentiles", "P", names(df)[iperc[order(perc)]])
                iperc <- sort(iperc)
                perc <- sort(perc)
                dfout <- cbind(df[, colStart, drop = FALSE],
                               df[, iperc[perc <= 50], drop = FALSE],
                               df[, "mean", drop = FALSE],
                               df[, iperc[perc > 50], drop = FALSE],df[, colEnd, drop = FALSE])
                if ((length(perc) == 3L) && all.equal(perc, c(25, 50, 75))) {
                    names(dfout)[names(dfout) == "P.25.0"] <- "q1"
                    names(dfout)[names(dfout) == "P.50.0"] <- "median"
                    names(dfout)[names(dfout) == "P.75.0"] <- "q3"
                }
                dfout
            }
            outputs <- names(x$outputs)
            out <- vector(mode = "list", length = length(outputs))
            for (i in 1:length(outputs)) {
                output <- outputs[i]
                y <- x$outputs[[i]]
                bands <- names(y$bands)
                out[[i]] <- do.call(rbind, lapply(names(y$bands), FUN = GetStatsParams, y, output))
            }
            if (aggregation_period == 1L) {
                interval <- data.frame(date = as.Date(x$interval$from))
            } else {
                interval <- data.frame(from = as.Date(x$interval$from), to = as.Date(x$interval$to) - 1)
            }
            outputs <- do.call(rbind, out)
            return(merge(interval, outputs))
        }
        return(do.call(rbind, lapply(lst, GetStatsOuput, aggregation_period)))
    } else {
        return(lst)
    }
}
