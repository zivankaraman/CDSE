#' @title Filter image catalog for seasonal images
#' @description Filters image catalog entries that fall in the season of interest -
#'    dates between \code{from} day/month and \code{to} day/month for all years in the
#'    \code{from} - \code{to} time range.
#' @param catalog \code{data.frame} or \code{sf} object as the one produced by a call to \code{SearchCatalog}
#' @param from start of the season of interest.
#' @param to end of the season of interest.
#'
#' The \code{from} and \code{to} arguments can be either Date or character that can be converted to date by \code{as.Date}.
#'     Open intervals are not allowed (both \code{from} and \code{to} must be valid dates).
#' @return A \code{data.frame} or a \code{sf} object, depending on the type of the input.
#' @examples
#' \dontrun{
#' dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
#' aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#' all_images <- SearchCatalog(aoi = aoi, from = "2021-06-01", to = "2023-08-31",
#'     collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
#' sesonal_images <- SeasonalFilter(all_images, from = "2021-06-01", to = "2023-08-31")
#' }
#' @seealso \code{\link[CDSE]{SearchCatalog}}, \code{\link[CDSE]{SeasonalTimerange}}
#' @rdname SeasonalFilter
#' @export
SeasonalFilter <- function(catalog, from, to) {
    # from <- as.Date(from)
    # to <- as.Date(to)
    # # Filter only dates between from day/month and to day/month for all years
    # to_date <- lubridate::ymd(paste(lubridate::year(from),
    #                                 lubridate::month(to),
    #                                 lubridate::day(to), sep = "-"))
    # # Find the lag in days between from date, and to date *for the same year*
    # per <- difftime(to_date, from, units = "days")
    # if (per < 0) {
    #     # In case the "season" crosses over to the next year
    #     to_date <- lubridate::ymd(paste(lubridate::year(from) + 1,
    #                                     lubridate::month(to),
    #                                     lubridate::day(to), sep = "-"))
    #     per <- difftime(to_date, from, units = "days")
    # }
    # # Loop over all years in the original time range
    # years <- seq.int(lubridate::year(from), lubridate::year(to))
    # out_list <- lapply(years, function(y) {
    #     per_start <- lubridate::ymd(paste(y, lubridate::month(from),
    #                                       lubridate::day(from),
    #                                       sep = "-"))
    #     per_end <- per_start + lubridate::days(per)
    #     out_df <- catalog[catalog$acquisitionDate >= per_start &
    #                           catalog$acquisitionDate <= per_end, ]
    #     return(out_df)
    # })
    # out_seasonal <- do.call(rbind, out_list)
    # # sort by acquisition date, newest to oldest
    # out_seasonal <- out_seasonal[rev(order(out_seasonal$acquisitionDate)), ]
    seasons <- SeasonalTimerange(from = from, to = to)
    out_list <- lapply(seasons, function(s) {
        out_df <- catalog[catalog$acquisitionDate >= s[1] &
                              catalog$acquisitionDate <= s[2], ]
        return(out_df)
    })
    out_seasonal <- do.call(rbind, out_list)
    # sort by acquisition date, newest to oldest
    out_seasonal <- out_seasonal[rev(order(out_seasonal$acquisitionDate)), ]
    # force default row.names
    row.names(out_seasonal) <- NULL
    return(out_seasonal)
}

#' @title Create seasonal time range
#' @description Creates list of seasonal filters (one per year) for the season of interest -
#'    dates between \code{from} day/month and \code{to} day/month for all years in the
#'    \code{from} - \code{to} time range.
#' @param from start of the season of interest.
#' @param to end of the season of interest.
#'
#' The \code{from} and \code{to} arguments can be either Date or character that can be converted to date by \code{as.Date}.
#'     Open intervals are not allowed (both \code{from} and \code{to} must be valid dates).
#' @return A list of time ranges defining the season of interest for each year.
#' @examples
#' \dontrun{
#' seasons <- SeasonalTimerange(from = "2020-05-01", to = "2023-09-30")
#' seasons <- SeasonalTimerange(from = "2019-11-01", to = "2023-03-30")
#' }
#' @rdname SeasonalTimerange
#' @export
SeasonalTimerange <- function(from, to) {
    # from <- as.Date(from)
    # to <- as.Date(to)
    # # Filter only dates between from day/month and to day/month for all years
    # to_date <- lubridate::ymd(paste(lubridate::year(from),
    #                                 lubridate::month(to),
    #                                 lubridate::day(to), sep = "-"))
    # # Find the lag in days between from date, and to date *for the same year*
    # per <- difftime(to_date, from, units = "days")
    # if (per < 0) {
    #     # In case the "season" crosses over to the next year
    #     to_date <- lubridate::ymd(paste(lubridate::year(from) + 1,
    #                                     lubridate::month(to),
    #                                     lubridate::day(to), sep = "-"))
    #     per <- difftime(to_date, from, units = "days")
    # }
    # # Loop over all years in the original time range
    # years <- seq.int(lubridate::year(from), lubridate::year(to))
    # out_list <- lapply(years, function(y) {
    #     per_start <- lubridate::ymd(paste(y, lubridate::month(from),
    #                                       lubridate::day(from),
    #                                       sep = "-"))
    #     per_end <- per_start + lubridate::days(per)
    #     return(c(per_start, per_end))
    # })
    # names(out_list) <- years

    from <- as.Date(from)
    to <- as.Date(to)
    # Filter only dates between from day/month and to day/month for all years
    to_date <- lubridate::ymd(paste(lubridate::year(from),
                                    lubridate::month(to),
                                    lubridate::day(to), sep = "-"))
    first_year <- lubridate::year(from)
    # Find the lag in days between from date, and to date *for the same year*
    lag <- difftime(to_date, from, units = "days")
    if (lag < 0) {
        # In case the "season" crosses over to the next year
        last_year <- lubridate::year(to) - 1L
        per_end_adj <- 1L
    } else {
        last_year <- lubridate::year(to)
        per_end_adj <- 0L
    }
    # Loop over all years in the original time range
    years <- seq.int(first_year, last_year)
    out_list <- lapply(years, function(y) {
        per_start <- lubridate::ymd(paste(y, lubridate::month(from),
                                          lubridate::day(from),
                                          sep = "-"))
        per_end <- lubridate::ymd(paste(y + per_end_adj, lubridate::month(to),
                                        lubridate::day(to),
                                        sep = "-"))
        return(c(per_start, per_end))
    })
    if (per_end_adj == 1L) {
        # If the "season" crosses over to the next year
        names(out_list) <- paste(years, years + 1L, sep = "/")
    } else {
        names(out_list) <- years
    }
    return(out_list)
}


#
# library(CDSE)
# id <- Sys.getenv("CDSE_ID")
# secret <- Sys.getenv("CDSE_SECRET")
# OAuthClient <- GetOAuthClient(id = id, secret = secret)
# dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
# aoi <- sf::read_sf(dsn, as_tibble = FALSE)
#
# first <- "2020-12-01"
# last <- "2023-01-31"
#
# catalog <- SearchCatalog(aoi = aoi, from = first, to = last,
#                         collection = "sentinel-2-l2a", with_geometry = TRUE,
#                         client = OAuthClient)
#
# filter = cql2_json(`eo:cloud_cover` < 20)
# filter = cql2_text(`eo:cloud_cover` < 0.01)
#
#
# filter = "sar:instrument_mode = 'IW' AND sat:orbit_state = 'ascending' AND s1:polarization = 'DV'"
# catalog <- SearchCatalog(aoi = aoi, from = first, to = last,
#                          collection = "sentinel-1-grd", filter = filter,
#                          with_geometry = TRUE, client = OAuthClient)
#
# catalog <- SearchCatalog(aoi = aoi, from = first, to = last,
#                          collection = "sentinel-1-grd", filter = NULL,
#                          with_geometry = FALSE, as_data_frame = FALSE, client = OAuthClient)
# x=catalog[[1]]
# sapply(catalog, function(x) x$properties$`sat:orbit_state`)
# sapply(catalog, function(x) x$properties$`s1:polarization`)
#
# catalog <- SearchCatalog(aoi = aoi, from = first, to = last,
#                          collection = "sentinel-2-l2a", filter = "eo:cloud_cover < 0.01",
#                          with_geometry = TRUE, client = OAuthClient)
#
# seasonal1 <- SeasonalFilter(catalog, from = first, to = last)
#
# lstTimeRange <- SeasonalTimerange(first, last)
#
# out_list <- lapply(lstTimeRange, function(timeRange, dt) {
#     subset(dt, acquisitionDate >= timeRange[1] & acquisitionDate <= timeRange[2])
# }, catalog)
#
# out_seasonal <- do.call(rbind, out_list)
# # sort by acquisition date, newest to oldest
# out_seasonal <- out_seasonal[rev(order(out_seasonal$acquisitionDate)), ]
# # force default row.names
# row.names(out_seasonal) <- NULL
#
# all.equal(seasonal1, out_seasonal)
#
#
#
# seasonal <- SearchCatalog(aoi = aoi, from = first, to = last,
#                          collection = "sentinel-2-l2a", with_geometry = TRUE,
#                          client = OAuthClient) |>
#     SeasonalFilter(from = first, to = last)
#
#
#
#
#
#
