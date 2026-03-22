#' @title Set API Path Structure
#' @description Switch between the legacy and the new API path structure for Sentinel Hub services
#' on the Copernicus Data Space Ecosystem.
#' @param new logical, indicating whether to use the new API path structure. Default is \code{FALSE} (uses legacy format).
#' @return No return value, called for side effects (sets the API endpoints).
#' @details As of March 2026, the Sentinel Hub APIs on the Copernicus Data Space Ecosystem support an additional format alongside the legacy paths.
#' \itemize{
#'   \item Legacy format: \code{/api/<version>/<service>}
#'   \item New format: \code{/<service>/<version>}
#' }
#' Setting \code{new = TRUE} switches the URLs for process and statistics API endpoints to the new format.
#' For more details, see \url{https://dataspace.copernicus.eu/news/2026-3-9-api-path-structure-updates-sentinel-hub-services}.
#' @examples
#' \dontrun{
#' SetPathAPI(new = TRUE)
#' }
#' @export
SetPathAPI <- function(new = FALSE) {
    if (isTRUE(new)) {
        options(CDSE.process_url = "https://sh.dataspace.copernicus.eu/process/v1",
                CDSE.statistical_url = "https://sh.dataspace.copernicus.eu/statistics/v1")
    } else {
        options(CDSE.process_url = "https://sh.dataspace.copernicus.eu/api/v1/process",
                CDSE.statistical_url = "https://sh.dataspace.copernicus.eu/api/v1/statistics")
    }
}
