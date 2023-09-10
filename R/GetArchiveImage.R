#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aoi PARAM_DESCRIPTION
#' @param bbox PARAM_DESCRIPTION
#' @param time_range PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param script PARAM_DESCRIPTION
#' @param format PARAM_DESCRIPTION
#' @param mosaicking_order PARAM_DESCRIPTION, Default: c("mostRecent", "leastRecent", "leastCC")
#' @param file PARAM_DESCRIPTION, Default: NULL
#' @param pixels PARAM_DESCRIPTION
#' @param resolution PARAM_DESCRIPTION
#' @param buffer PARAM_DESCRIPTION, Default: 0
#' @param mask PARAM_DESCRIPTION, Default: FALSE
#' @param client PARAM_DESCRIPTION
#' @param token PARAM_DESCRIPTION
#' @param url PARAM_DESCRIPTION, Default: 'https://services.sentinel-hub.com/api/v1/process'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[sf]{st_transform}}, \code{\link[sf]{st_geometry}}, \code{\link[sf]{st_bbox}}, \code{\link[sf]{geos_unary}}, \code{\link[sf]{st_coordinates}}
#'  \code{\link[geojsonsf]{sfc_geojson}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_headers}}, \code{\link[httr2]{req_body}}, \code{\link[httr2]{req_auth_bearer_token}}, \code{\link[httr2]{req_oauth_client_credentials}}, \code{\link[httr2]{req_perform}}
#'  \code{\link[terra]{rast}}, \code{\link[terra]{crs}}, \code{\link[terra]{project}}, \code{\link[terra]{mask}}, \code{\link[terra]{writeRaster}}
#' @rdname GetArchiveImage
#' @export 
#' @source \url{http://somewhere.important.com/}
#' @importFrom sf st_transform st_geometry st_bbox st_buffer st_coordinates st_centroid
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_headers req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform
#' @importFrom terra rast crs project mask writeRaster
GetArchiveImage <- function(aoi, bbox, time_range, type, script, format, mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
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
            type = type)
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
