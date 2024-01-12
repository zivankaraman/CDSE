# Initialization -----------------------------------------------------------------------

library(sf)
library(CDSE)


# Getting help about the package -------------------------------------------------------

help(package = "CDSE")


# Getting collection (doesn't require authentication) ----------------------------------

collections <- GetCollections()
collections


# Authenticate -------------------------------------------------------------------------

id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
OAuthClient <- GetOAuthClient(id = id, secret = secret)


# Search catalog -----------------------------------------------------------------------

# search for available Sentinel 2 L2A imagery of New York Central Park in July 2023
# get the New York City Central Park shape as area of interest
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
# search by area of interest
images <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31", collection = "sentinel-2-l2a",
                        with_geometry = TRUE, client = OAuthClient)
summary(images)
View(images)

# search for available Sentinel 2 L2A imagery of Luxembourg in July 2023
# get the Luxembourg country shape as area of interest
dsn <- system.file("extdata", "luxembourg.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31",
                        collection = "sentinel-2-l2a", with_geometry = TRUE,
                        client = OAuthClient)
View(images)

# plot AOI (Luxembourg) image tiles coverage
library(maps)
days <- range(as.Date(images$acquisitionDate))
maps::map(database = "world", col = "lightgrey", fill = TRUE, mar = c(0, 0, 4, 0),
          xlim = c(3, 9), ylim = c(47.5, 51.5))
plot(sf::st_geometry(aoi), add = TRUE, col = "red", border = FALSE)
plot(sf::st_geometry(images), add = TRUE)
title(main = sprintf("AOI coverage by image tiles for period %s",
                     paste(days, collapse = " / ")), line = 1L)

# summary of AOI coverage
summary(images$areaCoverage)
hist(images$areaCoverage)

# summary by tileNumber
# https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/naming-convention
tileNumber <- substring(images$sourceId, 39, 44)
by(images$areaCoverage, INDICES = tileNumber, FUN = summary)
lattice::histogram(~ areaCoverage | tileNumber,
                   data = data.frame(tileNumber, areaCoverage = images$areaCoverage),
                   type = "count", breaks = seq(0, 100, by = 2), col = "#00AFBB")

# search for available Sentinel 1 GRD imagery of Luxembourg in January 2019
# get the Luxembourg country shape as area of interest
dsn <- system.file("extdata", "luxembourg.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
# search by bounding box
bbox <- as.numeric(sf::st_bbox(aoi))
images <- SearchCatalog(bbox = bbox, from = "2019-01-01", to = "2019-01-30", collection = "sentinel-1-grd",
                        with_geometry = TRUE, client = OAuthClient)
summary(images)
View(images)

images <- SearchCatalog(bbox = bbox, from = "2019-01-01", to = "2019-01-30", collection = "sentinel-1-grd",
                        with_geometry = FALSE, client = OAuthClient)
summary(images)
