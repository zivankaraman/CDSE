#' @title Search collection for available images
#' @description Searches the specified collection for available images in the given time interval and intersecting with the
#'     bounding box or the area of interest.
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#'     Coordinates need to be in longitude, latitude.
#'
#' Only one of either \code{aoi} or \code{bbox} may be specified.
#' @param from start of the time interval to search.
#' @param to  end of the time interval to search.
#'
#' \code{from} and \code{to} can be either Date or character that can be converted to date by \code{as.Date}.
#'
#' Open interval (one side only) can be obtained by providing the \code{NA} or \code{NULL} value for the corresponding argument.
#' @param collection character indicating which collection to search.
#'     Must be one of the collections returned by \code{GetCollections}.
#' @param as_data_frame logical indicating if the result should be returned as data frame. Default: TRUE
#' @param with_geometry logical indicating if the granule geometries should be included in the data.frame. Default: TRUE
#' @param filter character, CQL2 text filter. Use the function \code{GetQueryables} to find out which filters
#'      can bu used with the collection. Default: NULL (no filtering)
#' @param client OAuth client object to use for authentication.
#' @param token OAuth token character string to use for authentication.
#'
#' Exactly one of either \code{client} or \code{token} must be specified. It is recommended to use \code{client}.
#' @param url character indicating the STAC catalog search endpoint. Default: Copernicus Data Space Ecosystem STAC endpoint
#' @return A \code{list}, \code{data.frame} or a \code{sf} object.
#' @details If no images found, a \code{NULL} value is returned.
#' @examples
#' \dontrun{
#' dsn <- system.file("extdata", "luxembourg.geojson", package = "CDSE")
#' aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#' images <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31",
#'           collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
#' images_cloudless <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31",
#'           filter = "eo:cloud_cover < 5",
#'           collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
#' }
#' @seealso
#'  \code{\link[CDSE]{GetCollections}}, \code{\link[CDSE]{GetQueryables}}, \code{\link[CDSE]{GetImage}}
#' @rdname SearchCatalog
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Catalog.html}
#' @importFrom sf st_transform st_geometry st_bbox st_polygon st_as_sfc st_intersects st_area st_intersection
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform resp_body_json
#' @importFrom lutz tz_lookup_coords
#' @importFrom lubridate with_tz
SearchCatalog <- function(aoi, bbox, from, to, collection, as_data_frame = TRUE, with_geometry = TRUE,
                          filter = NULL, client, token, url = getOption("CDSE.catalog_url")) {
    # Only one of either intersects or bbox may be specified.
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
    # determine the requested period
    if (is.na(from) || is.null(from)) {
        p1 <- ".."
    } else {
        p1 <- sprintf("%sT00:00:00Z", as.Date(from))
    }
    if (is.na(to) || is.null(to)) {
        p2 <- ".."
    } else {
        p2 <- sprintf("%sT23:59:59Z", as.Date(to))
    }
    period <- paste(p1, p2, sep = "/")
    # check period
    if (period == "../..") {
        stop("only one side of the time interval can be open")
    }
    limes <- 100 # 100 is the maximal allowed value
    # request body
    if (missing(aoi)) { # query by bbox
        # trick to deal with the fact that 'collections' should be boxed but 'limit' unboxed
        # -> wrap collections in a list and use auto_unbox = TRUE
        bdy <- list("bbox" = bbox, "datetime" = period, "collections" = list(collection), "limit" = limes)
    } else { # query by aoi / intersects
        aoi <- CheckAOI(aoi)
        # convert to WGS84 first to get longitude/latitude coordinates
        bounds <- sf::st_transform(sf::st_geometry(aoi), crs = 4326)
        # bounding box
        bbox <- as.numeric(sf::st_bbox(bounds))
        # intersects
        geom <- geojsonsf::sfc_geojson(sf::st_geometry(bounds))
        # trick to deal with the fact that 'collections' should be boxed but 'limit' unboxed
        # -> wrap collections in a list and use auto_unbox = TRUE
        bdy <- list("intersects" = jsonlite::fromJSON(geom), "datetime" = period, "collections" = list(collection), "limit" = limes)
    }
    # filter
    if (!is.null(filter)) {
        bdy$filter <- filter
        bdy$`filter-lang` <- "cql2-text"
    }
    # build the request
    req <- httr2::request(paste0(url, "search"))
    req <- httr2::req_body_json(req, data = bdy, auto_unbox = TRUE)

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
    cnt <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    features <- cnt$features
    # if no features found return NULL
    if (length(features) == 0L) return(NULL)
    # pagination
    while (!is.null(cnt$context$`next`)) {
        bdy$`next` <- cnt$context$`next`
        req <- httr2::req_body_json(req, data = bdy, auto_unbox = TRUE)
        resp <- httr2::req_perform(req)
        cnt <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        features <- c(features, cnt$features)
    }
    # prepare output
    if (as_data_frame) {
        datetime_txt <- sapply(features, FUN = function(x) x$properties$datetime)
        datetime_utc <- as.POSIXct(substr(datetime_txt, 1, 19), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
        centre <- apply(matrix(bbox, ncol = 2), 1, mean)
        tz <- lutz::tz_lookup_coords(lon = centre[1], lat = centre[2], method = "fast", warn = FALSE)
        datetime_tz <- lubridate::with_tz(datetime_utc, tzone = tz)
        bboxmat <- data.frame(t(sapply(features, FUN = function(x) as.numeric(x$bbox))))
        names(bboxmat) <- c("long.min", "lat.min", "long.max", "lat.max")
        out <- data.frame(
            acquisitionDate = as.Date(datetime_utc),
            tileCloudCover = sapply(features, FUN = function(x) SafeNull(x$properties$`eo:cloud_cover`)),
            satellite = sapply(features, FUN = function(x) x$properties$platform),
            acquisitionTimestampUTC = datetime_utc,
            acquisitionTimestampLocal = datetime_tz,
            sourceId = sapply(features, FUN = function(x) x$id),
            stringsAsFactors = FALSE)
        out <- cbind(out, bboxmat)
        if (with_geometry) {
            lst1 <- lapply(features, FUN = function(x) matrix(unlist(x$geometry$coordinates), ncol = 2, byrow = TRUE))
            lst2 <- lapply(1:length(lst1), FUN = function(i) sf::st_polygon(lst1[i]))
            geom_col <- sf::st_as_sfc(lst2, crs = 4326)
            if (missing(aoi)) {
                bounds <- PolyFromBbox(bbox)
            }
            doIntersect <- unlist(sf::st_intersects(sf::st_geometry(bounds), geom_col))
            # round to avoid areaCoverage not equal 100
            out[doIntersect, "areaCoverage"] <- round(100.0 *
                sf::st_area(sf::st_intersection(sf::st_geometry(bounds), geom_col)) /
                sf::st_area(sf::st_geometry(bounds)), 3)
            sf::st_geometry(out) <- geom_col
            col_ord <- c("acquisitionDate", "tileCloudCover",  "areaCoverage", "satellite",
                         "acquisitionTimestampUTC", "acquisitionTimestampLocal", "sourceId",
                         "long.min", "lat.min", "long.max", "lat.max", "geometry")
            out <- out[, col_ord]
        }

    } else {
        out <- features
    }
    return(out)
}
