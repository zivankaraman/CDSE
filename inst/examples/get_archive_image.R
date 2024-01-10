# Initialization -----------------------------------------------------------------------

library(sf)
library(CDSE)


# Getting help about the package -------------------------------------------------------
help(package = "CDSE")


# Getting collection doesn't require authentication ------------------------------------

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
images

# select the date with the least cloud cover, and retrieve the NDVI values
day <- images[order(images$tileCloudCover), ]$acquisitionDate[1]
script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
ras <- GetArchiveImage(aoi = aoi, time_range = day, script = script_file,
                       collection = "sentinel-2-l2a", format = "image/tiff",
                       mosaicking_order = "leastCC", resolution = 10,
                       mask = TRUE, buffer = 100, client = OAuthClient)
ras
ras[ras < 0] <- 0
terra::plot(ras, main = paste("Central Park NDVI on", day),
            col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))


# get the RGB image (PNG file)
bbox <- as.numeric(sf::st_bbox(aoi))
script_text <- paste(readLines(system.file("scripts", "TrueColorS2L2A.js", package = "CDSE")), collapse = "\n")
cat(script_text, sep = "\n")
png <- tempfile("img", fileext = ".png")
GetArchiveImage(bbox = bbox, time_range = day, script = script_text,
                collection = "sentinel-2-l2a", file = png, format = "image/png",
                mosaicking_order = "leastCC", pixels = c(600, 950), client = OAuthClient)
terra::plotRGB(terra::rast(png))

# get the same image as raster object
ras <- GetArchiveImage(aoi = aoi, time_range = day, script = script_text,
                collection = "sentinel-2-l2a", file = NULL, format = "image/png",
                mosaicking_order = "leastCC", pixels = c(600, 950), client = OAuthClient)
terra::plotRGB(ras)

# the fourth band contains data mask, can use it to cut background
terra::plot(ras)
x <- terra::mask(ras[[1:3]], ras[[4]], maskvalue = 0)
terra::plotRGB(x)
