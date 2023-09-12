#' @title List available collections
#' @description Retrieves the list of available imagery collections.
#' @param as_data_frame logical indicating if the result should be returned as data frame. Default: TRUE
#' @param url character indicating the STAC catalog search endpoint. Default: Copernicus Data Space Ecosystem STAC endpoint
#' @return A \code{list} or a \code{data.frame} of all available imagery collections and their attributes.
#' @details This function doesn't require authentication.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[CDSE]{GetArchiveImage}}, \code{\link[CDSE]{SearchCatalog}}
#' @rdname GetCollections
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Catalog.html}
#' @importFrom httr2 request req_perform resp_body_json
GetCollections <- function(as_data_frame = TRUE, url = "https://sh.dataspace.copernicus.eu/api/v1/catalog/1.0.0/") {
    req <- httr2::request(paste0(url, "collections"))
    resp <- httr2::req_perform(req)
    if (isTRUE(as_data_frame)) {
        cnt <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        collezioni <- cnt$collections
        out <- data.frame(
            id = collezioni$id,
            title = collezioni$title,
            description = collezioni$description,
            since = sapply(collezioni$extent$temporal$interval, "[", 1),
            instrument = unlist(collezioni$summaries$instrument),
            gsd = sapply(collezioni$summaries$gsd, FUN = function(x) SafeNull(x)),
            bands = sapply(collezioni$summaries$`eo:bands`, FUN = function(x) ifelse(is.null(x), NA, nrow(x))),
            constellation = sapply(collezioni$summaries$constellation, FUN = function(x) SafeNull(x)),
            bbox = I(matrix(unlist(collezioni$extent$spatial$bbox), ncol = 4, byrow = TRUE)),
            stringsAsFactors = FALSE, row.names = NULL)
        } else {
            cnt <- httr2::resp_body_json(resp, simplifyVector = FALSE)
            out <- cnt$collections
    }
    return(out)
}
