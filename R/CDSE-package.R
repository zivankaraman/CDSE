#' @name CDSE-package
#' @aliases CDSE
#' @docType package
#' @author Zivan Karaman <zivan.karaman@gmail.com>
# @seealso \link{ca630}, \code{\link{sp1}, \link{sp2}, \link{sp3}, \link{sp4}, \link{sp5}}
#'
#' @title Package providing interface to the 'Copernicus Data Space Ecosystem' API
#'
#' @description
#'
#' The \code{CDSE} package for R was developed to allow access to the
#' 'Copernicus Data Space Ecosystem' \url{https://dataspace.copernicus.eu/}
#' data and services from R. The 'Copernicus Data Space Ecosystem',
#' deployed in 2023, offers access to the EO data collection from the Copernicus missions,
#' with discovery and download capabilities and numerous data processing tools.
#' In particular, the 'Sentinel Hub' API \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub.html}
#' provides access to the multi-spectral and multi-temporal big data satellite imagery service,
#' capable of fully automated, real-time processing and distribution of remote sensing data and related EO products.
#' Users can use APIs to retrieve satellite data over their AOI and specific time range from
#' full archives in a matter of seconds. When working on the application of EO where the area
#' of interest is relatively small compared to the image tiles distributed by Copernicus (100 x 100 km),
#' it allows to retrieve just the portion of the image of interest rather than downloading the huge tile
#' image file and processing it locally. The goal of the \code{CDSE} package is to provide
#' easy access to this functionality from R.
#'
#' The main functions allow to search the catalog of available imagery from the Sentinel-1, Sentinel-2,
#' Sentinel-3, and Sentinel-5 missions, and to process and download the images of an area of interest and
#'  a time range in various formats. Other functions might be added in subsequent releases of the package.
#'
#'
#' @section API authentication:
#' Most of the API functions require OAuth2 authentication. The recommended procedure is to obtain
#' an authentication client object from the \code{GetOAuthClient} function, and to pass it as the \code{client}
#' argument to the functions requiring the authentication. For more detailed information, you are
#' invited to consult the \code{"Before you start"} document.
#'
#' @section Project homepage:
#' \url{https://github.com/zivankaraman/CDSE}
#'
#' @section Issues:
#' For bug reports and feature requests please use the tracker \url{https://github.com/zivankaraman/CDSE/issues}
#'
#'
## @keywords internal
##"_PACKAGE"
NULL
