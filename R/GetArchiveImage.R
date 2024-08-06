#' @name GetArchiveImage
#' @title Get image from the archive (deprecated)
#' @description Retrieves the image for the area of interest using the parameters provided.
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#'     Coordinates need to be in longitude, latitude.
#'
#' Only one of either \code{aoi} or \code{bbox} may be specified.
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
#' @return \code{SpatRaster} object (from the package \code{terra}) of the requested image (if \code{file} is \code{NULL}),
#'     or the (invisible) name of the file created.
#' @details If \code{aoi} argument is provided, the result is returned in the same coordinate reference system.
#' @examples
#' \dontrun{
#' dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
#' aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#' script_file <- system.file("scripts", "NDVI_uint8.js", package = "CDSE")
#' day <- "2023-07-11"
#' ras <- GetArchiveImage(aoi = aoi, time_range = day, script = script_file,
#'        collection = "sentinel-2-l2a",format = "image/tiff",
#'        mosaicking_order = "leastCC", resolution = 10, client = OAuthClient)
#'}
#' @seealso \code{\link[CDSE]{GetCollections}}, \code{\link[CDSE]{SearchCatalog}}
#' @seealso \code{\link{CDSE-deprecated}}
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Process.html}
#' @importFrom sf st_transform st_geometry st_bbox st_buffer st_coordinates st_centroid
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform
#' @importFrom terra rast crs project mask writeRaster
NULL

#' @name CDSE-deprecated
#' @usage NULL
#' @aliases NULL
#' @section \code{GetArchiveImage}:
#' For \code{GetArchiveImage}, use \code{\link{GetImage}}.
#' @export
GetArchiveImage <- function(aoi, bbox, time_range, collection, script, file = NULL,
                            format = c("image/tiff", "image/png", "image/jpeg"),
                            mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
                            pixels, resolution, buffer = 0, mask = FALSE,
                            client, token, url = getOption("CDSE.process_url")) {
    .Deprecated("GetImage")
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

    # get the first (default) value
    mosaicking_order <- mosaicking_order[1]
    format <- format[1]

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
    # get the time range
    period <- MakeTimeRange(time_range)
    data <- list(
        list(
            dataFilter = list(timeRange = period, mosaickingOrder = mosaicking_order[1]),
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
    responses <- list(list(identifier = "default", format = list(type = format[1])))
    # build output part of the request
    if (missing(resolution)) { # use width and height provided
        # check the size of pixels
        yc <- mean(c(bbox[2], bbox[4]))
        dummy_rast <- terra::rast(nrows = pixels[2], ncols =  pixels[1], crs = sf::st_crs(bounds),
                                  extent = terra::ext(bbox[c(1, 3, 2, 4)]))
        pixSize <- terra::res(dummy_rast) * DegLength(yc)
        pixSize <- pixSize * 1.001 # adjust for small discrepancy between SH and our values
        if (any(pixSize > 1500)) {
            msg <- sprintf("The requested pixel size (%.1f x %.1f) exceeds the allowed maximum (1500 meters).", pixSize[1], pixSize[2])
            msg <- c(msg, "\nPlease revise the number of pixels (image width/height) to make sure the pixel size is in supported range.")
            warning(msg)

        }
        output <- list(width = pixels[1], height = pixels[2],
                       responses = responses)
    } else {
        # compute the resolution at the latitude of the centroid
        lat <- sf::st_coordinates(sf::st_centroid(bounds))[1, "Y"]
        res <- resolution / DegLength(lat)
        # check the number of pixels
        yc <- mean(c(bbox[2], bbox[4]))
        dummy_rast <- terra::rast(xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4],
                                  crs = sf::st_as_text(sf::st_crs(bounds)), resolution = resolution / DegLength(yc))
        dims <- dim(dummy_rast)[1:2]
        if (any(dims > 2500)) {
            msg <- sprintf("The requested image dimension (%d x %d) exceeds the allowed maximum (2500 pixels).", dims[1], dims[2])
            msg <- c(msg, "\nPlease revise the resolution to make sure it is in supported range.")
            warning(msg)
        }
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
        if (length(grep("SSL peer certificate", resp[1])) == 1L) {
            req <- httr2::req_options(req, ssl_verifyhost = 0L, ssl_verifypeer = 0L)
            resp <- httr2::req_perform(req)
            warning("Host SSL certificate seems to have an issue (probably expired)")
        } else {
            stop(LastError())
        }
    }
    if (unlist(strsplit(format, split = "/", fixed = TRUE))[1] != "image") {
        # TBD - process multipart response
        msg <- "Sorry, multiple responses requests are not handled (yet)."
        stop(msg)
    } else {
        if ((format != "image/tiff")) {    # JPEG or PNG image
            if (is.null(file)) {
                tmpfic <- tempfile()
                writeBin(resp$body, tmpfic)
                # read the file as raster
                ras <- suppressWarnings(terra::rast(tmpfic)) # warning if not tiff file
                return(ras)
            } else {
                writeBin(resp$body, file)
                invisible(file)
            }
        } else {
            # write to temporary file to post-process the image
            tmpfic <- tempfile()
            writeBin(resp$body, tmpfic)
            # read the file to transform in original CRS
            ras <- terra::rast(tmpfic)
            # if only bbox provided or format not tiff this can't be done
            if (!missing(aoi)) {
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
}
