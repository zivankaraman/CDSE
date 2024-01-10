## ----label = "knitr options", include = FALSE---------------------------------
knitr::opts_chunk$set(
    fig.width = 7,
    fig.height = 4,
    out.width = "100%",
    fig.align = "center",
    collapse = TRUE,
    comment = "#>"
)

## ----label = "setup", include = FALSE-----------------------------------------
library(CDSE)
options(warn = -1)

## ----label = "GetOAuthClient"-------------------------------------------------
id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
OAuthClient <- GetOAuthClient(id = id, secret = secret)

## ----label = "get collections"------------------------------------------------
collections <- GetCollections(as_data_frame = TRUE)
collections

## ----label = "search catalog"-------------------------------------------------
dsn <- system.file("extdata", "luxembourg.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31", 
                        collection = "sentinel-2-l2a", with_geometry = TRUE, 
                        client = OAuthClient)
images

## ----label = "turn off global device", include = FALSE------------------------
knitr::opts_knit$set(global.device = FALSE)

## ----label = "plot AOI coverage", fig.cap = "Luxembourg image tiles coverage"----
library(maps)
days <- range(as.Date(images$acquisitionDate))
maps::map(database = "world", col = "lightgrey", fill = TRUE, mar = c(0, 0, 4, 0),
          xlim = c(3, 9), ylim = c(47.5, 51.5))
plot(sf::st_geometry(aoi), add = TRUE, col = "red", border = FALSE)
plot(sf::st_geometry(images), add = TRUE)
title(main = sprintf("AOI coverage by image tiles for period %s", 
                     paste(days, collapse = " / ")), line = 1L)

## ----label = "summary AOI coverage"-------------------------------------------
summary(images$areaCoverage)

## ----label = "by tileNumber"--------------------------------------------------
tileNumber <- substring(images$sourceId, 39, 44)
by(images$areaCoverage, INDICES = tileNumber, FUN = summary)

## ----label = "search catalog to select image"---------------------------------
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2021-05-01", to = "2021-05-31", 
                        collection = "sentinel-2-l2a", with_geometry = TRUE, 
                        client = OAuthClient)
images
summary(images$areaCoverage)

## ----label = "retrieve the NDVI image", fig.cap = "Central Park NDVI raster"----
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


## ----label = "retrieve the RGB image", fig.cap = "Central Park image as PNG file", fig.width = 5, fig.height = 3----
bbox <- as.numeric(sf::st_bbox(aoi))
script_text <- paste(readLines(system.file("scripts", "TrueColorS2L2A.js", 
                                           package = "CDSE")), collapse = "\n")
cat(script_text, sep = "\n")
png <- tempfile("img", fileext = ".png")
GetArchiveImage(bbox = bbox, time_range = day, script = script_text, 
                collection = "sentinel-2-l2a", file = png, format = "image/png", 
                mosaicking_order = "leastCC", pixels = c(600, 950), client = OAuthClient)
terra::plotRGB(terra::rast(png))

