#' @title Produce image catalog without multiple entries per date
#' @description Sometimes several images could be available for the given day. It can be useful to have a list
#'     where for any given day there is just one row in the list. This unique row can be selected to represent either
#'     the least cloud coverage or the biggest coverage of the are of interest.
#' @param imageCatalog \code{data.frame} as returned by the \code{SearchCatalog} function
#' @param by character indicating which attribute is used to select the best image per date.
#'     Can be either "areaCoverage" or "tileCloudCover".
#' @param keep list of columns to keep in output. Default: all columns in input.
#' @return \code{data.frame} with one row per date.
#' @details The returned \code{data.frame} has the same columns as the input catalog.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[CDSE]{SearchCatalog}}
#' @rdname UniqueCatalog
#' @export
#' @importFrom stats aggregate
UniqueCatalog <- function(imageCatalog, by = c("areaCoverage", "tileCloudCover"), keep = names(imageCatalog)) {
    by <- match.arg(by, choices = c("areaCoverage", "tileCloudCover"))
    if (by == "areaCoverage") {
        # get image with maximal area coverage for the day
        agg1 <- stats::aggregate(areaCoverage ~ acquisitionDate, data = imageCatalog, FUN = max)
        tmp <- merge.data.frame(agg1, imageCatalog, by = c("acquisitionDate", "areaCoverage"), sort = FALSE)
    } else {
        # get image with minimal cloud cover for the day
        agg1 <- stats::aggregate(tileCloudCover ~ acquisitionDate, data = imageCatalog, FUN = min)
        tmp <- merge.data.frame(agg1, imageCatalog, by = c("acquisitionDate", "tileCloudCover"), sort = FALSE)
    }
    # in case of ties, get an arbitrary image (here the smallest sourceId, could also be the biggest)
    agg2 <- stats::aggregate(sourceId ~ acquisitionDate, data = tmp, FUN = min)
    tmp <- merge.data.frame(agg2, tmp, by = c("acquisitionDate", "sourceId"), sort = FALSE)
    out <- tmp[, keep]
    out <- out[rev(order(tmp$acquisitionDate)), ]
    # clear the row names
    row.names(out) <- NULL
    return(out)
}
