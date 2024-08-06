#' @title Get CQL2 parameters for a collection
#' @description Returns a list of variable terms that can be used in CQL2 expressions to filter the collection catalog search.
#' @param collection character indicating the collection for which the parameters are queried.
#'     Must be one of the collections returned by \code{GetCollections}.
#' @param as_data_frame logical indicating if the result should be returned as data frame. Default: TRUE
#' @param client OAuth client object to use for authentication.
#' @param token OAuth token character string to use for authentication.
#'
#' Exactly one of either \code{client} or \code{token} must be specified. It is recommended to use \code{client}.
#' @param url character indicating the STAC catalog search endpoint. Default: Copernicus Data Space Ecosystem STAC endpoint
#' @return A \code{list} or a \code{data.frame}.
#' @details If no parameters found, a \code{NULL} value or 0-row \code{data.frame} is returned.
#' @examples
#' \dontrun{
#' GetQueryables("sentinel-2-l2a", client = OAuthClient)
#' }
#' @seealso
#'  \code{\link[CDSE]{GetCollections}}, \code{\link[CDSE]{SearchCatalog}}
#' @rdname GetQueryables
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/ApiReference.html#tag/catalog_collections/operation/getCatalogCollectionQueryables}
#' @importFrom sf st_transform st_geometry st_bbox st_polygon st_as_sfc st_intersects st_area st_intersection
#' @importFrom geojsonsf sfc_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_body_json req_auth_bearer_token req_oauth_client_credentials req_perform resp_body_json
#' @importFrom lutz tz_lookup_coords
#' @importFrom lubridate with_tz
GetQueryables <- function(collection, as_data_frame = TRUE, client, token, url = getOption("CDSE.catalog_url")) {
    # build the request
    req <- httr2::request(sprintf("%scollections/%s/queryables", url, collection))
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
    out <- jsonlite::fromJSON(httr2::resp_body_string(resp, encoding = "UTF-8"))
    props <- out$properties
    if (as_data_frame) {
        SafeNull <- function(x) {
            ifelse(is.null(x), NA, x)
        }
        out <- data.frame(
            collection = rep(collection, length(props)),
            name = names(props),
            description = as.character(sapply(props, "[[", "description")),
            type = as.character(sapply(props, "[[", "type")),
            enum = as.character(sapply(props, FUN = function(x) paste(x[["enum"]], collapse = ","))),
            minimum = as.numeric(sapply(props, FUN = function(x) SafeNull(x[["minimum"]]))),
            maximum = as.numeric(sapply(props, FUN = function(x) SafeNull(x[["maximum"]]))),
            stringsAsFactors = FALSE, row.names = NULL)
    } else {
        out <- props
    }
    return(out)
}
