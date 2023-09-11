#' @title Get an image from the archive
#' @description FUNCTION_DESCRIPTION
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'
#' Only one of either aoi or bbox may be specified.
#' @param time_range scalar or vector (Date or character that can be converted to date) defining the time interval
#' @param collection character indicating which collection to search.
#'     Must be one of the collections returned by \code{GetCollections}.
#' @param script character with tha evaluation script or the name of the file containing the script
#' @param format output format (PNG, JPG, TIFF)
#' @param mosaicking_order PARAM_DESCRIPTION. Default: c("mostRecent", "leastRecent", "leastCC")
#' @param file name of the file to save the image. If NULL the raster object is returened. Default: NULL
#' @param pixels PARAM_DESCRIPTION
#' @param resolution PARAM_DESCRIPTION
#' @param buffer width of the buffer (in meters) to retrieve image of enlarged area. Default: 0
#' @param mask logical indicating if the image should contain only pixels within aoi. Default: FALSE
#' @param client OAuth client object to use for authentication.
#' @param token OAuth token character string to use for authentication.
#' @param url character indicating the process endpoint. Default: Copernicus Data Space Ecosystem process endpoint
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[CDSE]{GetCollections}}
#' @rdname GetArchiveImage
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Process.html}
#' @importFrom sf st_transform st_geometry st_bbox st_buffer st_coordinates st_centroid
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform
#' @importFrom terra rast crs project mask writeRaster
GetArchiveImage <- function(aoi, bbox, time_range, collection, script, format, mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                            file = NULL, pixels, resolution, buffer = 0, mask = FALSE,
                            client, token, url = "https://services.sentinel-hub.com/api/v1/process")
{
    # Only one of either aoi or bbox may be specified.
    if (!missing(aoi) & !missing(bbox)) {
        stop("Only one of either aoi or bbox may be specified.")
    }
    if (missing(aoi) & missing(bbox)) {
        stop("Either aoi or bbox must be specified.")
    }

    # Check bbox is valid
    if (!missing(bbox)) {
        CheckBbox(bbox)
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

    if (missing(aoi)) { # query by bbox
        # make bounds from bbox
        bounds <- PolyFromBbox(bbox)
    } else { # query by aoi / intersects
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

    # get the number of pixels required
    # boundsPlanar <- Flatten(bounds)
    # if (buffer > 0) {
    #     boundsPlanar <- sf::st_buffer(boundsPlanar, dist = buffer, joinStyle = "MITRE", mitreLimit = 999999)
    #     bbox <- as.numeric(sf::st_bbox(sf::st_transform(boundsPlanar, crs = 4326)))
    # }
    # dims <- apply(matrix(as.numeric(sf::st_bbox(boundsPlanar)), ncol = 2), 1, diff)
    # pixels <- ceiling(dims/resolution)
    #
    # dims.m <- apply(matrix(as.numeric(sf::st_bbox(boundsPlanar)), ncol = 2), 1, diff)
    # dims.d <- apply(matrix(as.numeric(bbox), ncol = 2), 1, diff)
    # res <- resolution * dims.d / dims.m

    # get the time range
    period <- MakeTimeRange(time_range)
    data <- list(
        list(
            dataFilter = list(timeRange = period, mosaickingOrder = priority),
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
    # responses
    # if (missing(responses)) {
        responses <- list(list(identifier = "default", format = list(type = format)))
    # }
    # build output part of the request
    if (missing(resolution)) { # use width and height provided
        output <- list(width = pixels[1], height = pixels[2],
                       responses = responses)
    } else {
        # compute the resolution at the latitude of the centroid
        lat <- sf::st_coordinates(sf::st_centroid(bounds))[1, "Y"]
        res <- resolution / DegLength(lat)
        output <- list(resx = res[1], resy = res[2],
                       responses = responses)
    }
    # read the evalscript from file if needed
    if (file.exists(script)) {
        script <- paste(readLines(script), collapse = "\n")
    }
    # make the request body
    bdy <- list(input = input, output = output, evalscript = script)

    # build the request
    req <- httr2::request(url)
    req <- httr2::req_headers(req, Accept = format)
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
        stop(LastError())
    }
    if (unlist(strsplit(format, split = "/", fixed = TRUE))[1] != "image") {
        # TBD - process multipart response
    } else {
        # write to temporary file to post-process the image
        tmpfic <- tempfile()
        writeBin(resp$body, tmpfic)
        # read the file to transform in original CRS
        ras <- suppressWarnings(terra::rast(tmpfic)) # warning if not tiff file
        # if only bbox provided or format not tiff this can't be done
        if (!missing(aoi) & terra::crs(ras) != "") {
            ras <- terra::project(ras, terra::crs(aoi))
            if (mask) {
                ras <- terra::mask(ras, aoi)
            }
        }
        # save to file or return the raster
        if (is.null(file)) {
            return(ras)
        } else {
            terra::writeRaster(ras, filename = file)
            invisible(file)
        }
    }
}
