#' @title Search collection for available images (vectorization ready)
#' @description These functions search the specified collection for available images using the parameters provided.
#'     They are simple wrappers around the 'SearchCatalog' function with arguments organized in a way
#'     that facilitates calling the function in a vectorized manner (using 'lapply' or similar function)
#'     and thus potentially also the parallelization. The 'from' and 'to' arguments are combined into a single argument 'time_range'.
#' @return A \code{list}, \code{data.frame} or a \code{sf} object.
#' @details If no images found, a \code{NULL} value is returned.
#' @examples
#' \dontrun{
#' dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
#' aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#' seasons <- SeasonalTimerange(from = "2020-06-01", to = "2023-08-31")
#' lst_images_cloudless <- lapply(seasons, SearchCatalogByTimerange, aoi = aoi,
#'     collection = "sentinel-2-l2a", with_geometry = FALSE,
#'     filter = "eo:cloud_cover < 5", client = OAuthClient)
#' images_cloudless <- do.call(rbind, lst_images_cloudless)
#' images_cloudless <- images_cloudless[rev(order(images_cloudless$acquisitionDate)), ]
#' row.names(images_cloudless) <- NULL
#' head(images_cloudless[, 1:5])
#' }
#' @seealso
#'  \code{\link[CDSE]{SearchCatalog}}
#' @rdname SearchCatalogBy...
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Catalog.html}

#' @name SearchCatalogBy...
#' @param time_range scalar or vector (Date or character that can be converted to date) defining the time interval.
#' Open interval (one side only) can be obtained by providing the \code{NA} or \code{NULL} value for the corresponding argument.

# @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
# @param bbox numeric vector of four elements describing the bounding box of interest.
#     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#     Coordinates need to be in longitude, latitude.
#
# Only one of either \code{aoi} or \code{bbox} may be specified.
#'
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


#' @details \code{SearchCatalogByTimerange} is arranged for vectorization on time_range (time_range is the first argument).
#' @export
SearchCatalogByTimerange <- function(time_range, aoi, bbox, collection, as_data_frame = TRUE, with_geometry = TRUE,
                                   filter = NULL, client, token, url = getOption("CDSE.catalog_url")) {
    period <- MakeTimeRange(time_range, format = FALSE)
    return(SearchCatalog(aoi = aoi, bbox = bbox, from = period$from, to = period$to, collection = collection,
                  as_data_frame = as_data_frame, with_geometry = with_geometry,
                  filter = filter, client = client, token = token, url = url))
}



#' @name SearchCatalogBy...
#' @param aoi sf or sfc object, typically a (multi)polygon, describing the Area of Interest.
#' @details \code{SearchCatalogByAOI} is arranged for vectorization on aoi (aoi is the first argument).
#' @export
SearchCatalogByAOI <- function(aoi, time_range, collection, as_data_frame = TRUE, with_geometry = TRUE,
                                    filter = NULL, client, token, url = getOption("CDSE.catalog_url")) {
    period <- MakeTimeRange(time_range, format = FALSE)
    return(SearchCatalog(aoi = aoi, from = period$from, to = period$to, collection = collection,
                  as_data_frame = as_data_frame, with_geometry = with_geometry,
                  filter = filter, client = client, token = token, url = url))
}

#' @name SearchCatalogBy...
#' @param bbox numeric vector of four elements describing the bounding box of interest.
#'     Specify with a coordinate pair on two (opposite) vertices of the bounding box rectangle.
#'     Coordinates need to be in longitude, latitude.
#'
#' Only one of either \code{aoi} or \code{bbox} may be specified.
#' @details \code{SearchCatalogByBbox} is arranged for vectorization on bbox (bbox is the first argument).
#' @export
SearchCatalogByBbox <- function(bbox, time_range, collection, as_data_frame = TRUE, with_geometry = TRUE,
                              filter = NULL, client, token, url = getOption("CDSE.catalog_url")) {
    period <- MakeTimeRange(time_range, format = FALSE)
    return(SearchCatalog(bbox = bbox, from = period$from, to = period$to, collection = collection,
                  as_data_frame = as_data_frame, with_geometry = with_geometry,
                  filter = filter, client = client, token = token, url = url))
}
