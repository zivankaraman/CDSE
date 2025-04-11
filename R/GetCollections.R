#' @title List available collections
#' @description Retrieves the list of available imagery collections.
#' @param as_data_frame logical indicating if the result should be returned as data frame. Default: TRUE
#' @param url character indicating the STAC catalog search endpoint. Default: Copernicus Data Space Ecosystem STAC endpoint
#' @return A \code{list} or a \code{data.frame} of all available imagery collections and their attributes.
#' @details This function doesn't require authentication.
#' @examples
#' \dontrun{
#' GetCollections(as_data_frame = TRUE)
#' }
#' @seealso \code{\link[CDSE]{GetImage}}, \code{\link[CDSE]{SearchCatalog}}
#' @rdname GetCollections
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Catalog.html}
#' @importFrom httr2 request req_perform resp_body_json
GetCollections <- function(as_data_frame = TRUE, url = getOption("CDSE.catalog_url")) {
    req <- httr2::request(paste0(url, "collections"))
    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
        if (length(grep("SSL peer certificate", resp[1])) == 1L) {
            req <- httr2::req_options(req, ssl_verifyhost = 0L, ssl_verifypeer = 0L)
            resp <- httr2::req_perform(req)
        } else {
            stop(LastError())
        }
    }
    if (isTRUE(as_data_frame)) {
        cnt <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        collezioni <- cnt$collections
        bbox <- data.frame(matrix(unlist(collezioni$extent$spatial$bbox), ncol = 4, byrow = TRUE))
        names(bbox) <- c("long.min", "lat.min", "long.max", "lat.max")
        out <- data.frame(
            id = collezioni$id,
            title = collezioni$title,
            description = collezioni$description,
            since = sapply(collezioni$extent$temporal$interval, "[", 1),
            instrument = sapply(collezioni$summaries$instrument, FUN = paste, collapse = "/"),
            gsd = sapply(collezioni$summaries$gsd, FUN = function(x) SafeNull(x)),
            bands = sapply(collezioni$summaries$`eo:bands`, FUN = function(x) ifelse(is.null(x), NA, nrow(x))),
            constellation = sapply(collezioni$summaries$constellation, FUN = function(x) SafeNull(x)),
            stringsAsFactors = FALSE, row.names = NULL)
        out <- cbind(out, bbox)
        } else {
            cnt <- httr2::resp_body_json(resp, simplifyVector = FALSE)
            out <- cnt$collections
    }
    return(out)
}
