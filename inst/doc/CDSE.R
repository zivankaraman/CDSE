## ----label = "setup", include = FALSE-----------------------------------------
library(sf)
library(terra)
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
    collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
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
    paste(days, collapse = " / ")), line = 1L, cex.main = 0.75)

## ----label = "summary AOI coverage"-------------------------------------------
summary(images$areaCoverage)

## ----label = "by tileNumber"--------------------------------------------------
tileNumber <- substring(images$sourceId, 39, 44)
by(images$areaCoverage, INDICES = tileNumber, FUN = summary)

## ----label = "seasonal filter on catalog"-------------------------------------
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2021-01-01", to = "2023-12-31", 
    collection = "sentinel-2-l2a", with_geometry = FALSE, filter = "eo:cloud_cover < 5", 
    client = OAuthClient)
dim(images)
summer_images <- SeasonalFilter(images, from = "2021-06-01", to = "2023-08-31")
dim(summer_images)

## ----label = "seasonal query on catalog"--------------------------------------
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
seasons <- SeasonalTimerange(from = "2021-06-01", to = "2023-08-31")
lst_summer_images <- lapply(seasons, SearchCatalogByTimerange, aoi = aoi, 
    collection = "sentinel-2-l2a", filter = "eo:cloud_cover < 5", with_geometry = FALSE, 
    client = OAuthClient)
summer_images <- do.call(rbind, lst_summer_images)
dim(summer_images)
summer_images <- summer_images[rev(order(summer_images$acquisitionDate)), ]
row.names(summer_images) <- NULL
head(summer_images)

## ----label="spectral indices"-------------------------------------------------
si <- rsi::spectral_indices() # get spectral indices
# NDVI
ndvi <- subset(si, short_name == "NDVI") # creates one-row data.frame
ndvi_script <- MakeEvalScript(ndvi) # generates the script
# GDVI
gdvi <- subset(si, short_name == "GDVI") # creates one-row data.frame
# GDVI requires an extra argument provided by the user
gdvi_script <- MakeEvalScript(gdvi, nexp = 2) # generates the script

## ----label="spectral indices by short_name"-----------------------------------
gdvi_script <- MakeEvalScript("GDVI", nexp = 2)

## ----label="custom spectral indices"------------------------------------------
custom_def <- list(bands = c("R", "G", "B"),
                   formula = "0.3 * R + 0.59 * G + 0.11 * B",
                   # long_name = "Greyscale image",
                   platforms = "Sentinel-2")
custom_script <- paste(MakeEvalScript(custom_def), collapse = "\n")

## ----label="compare greyscale and RGB images"---------------------------------
# select the day with smallest cloud cover
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2023-06-01", to = "2023-08-31",
                        collection = "sentinel-2-l2a", with_geometry = TRUE,
                        client = OAuthClient)
day <- images[order(images$tileCloudCover), ][["acquisitionDate"]][1]
# get the greyscale image
grey_file <- file.path(tempdir(), "grey.tif")
GetImage(bbox = sf::st_bbox(aoi), time_range = day, script = custom_script, file = grey_file,
         collection = "sentinel-2-l2a", format = "image/tiff",
         mosaicking_order = "leastCC", resolution = 20,
         mask = FALSE, buffer = 100, client = OAuthClient)
# get the RGB image
script_file <- system.file("scripts", "TrueColorS2L2A.js", package = "CDSE")
rgb_file <- file.path(tempdir(), "rgb.tif")
GetImage(bbox = sf::st_bbox(aoi), time_range = day, script = script_file, file = rgb_file,
         collection = "sentinel-2-l2a", format = "image/tiff",
         mosaicking_order = "leastCC", resolution = 20,
         mask = FALSE, buffer = 100, client = OAuthClient)
# Import the rasters
rgb_img <- terra::rast(rgb_file)
grey_img <- terra::rast(grey_file)
# Rescale greyscale raster values to 0 - 1 range
mM <- terra::minmax(grey_img)
grey_img <- (grey_img - mM[1])/(mM[2] - mM[1])
# Set up plotting window for side-by-side display
old.par <- par(mfrow = c(1, 2))
# Plot RGB image
plotRGB(rgb_img)   # expects layers 1,2,3 as R,G,B
# Plot greyscale image
plot(grey_img, col = grey.colors(256, start = 0, end = 1), legend = FALSE, axes = FALSE, mar = 0)

## ----label = "reset plot", include = FALSE------------------------------------
# Reset plotting layout to default
par(old.par)

## ----label = "search catalog to select image"---------------------------------
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2021-05-01", to = "2021-05-31", 
    collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
images
summary(images$areaCoverage)

## ----label = "retrieve the NDVI image", fig.cap = "Central Park NDVI raster"----
day <- images[order(images$tileCloudCover), ]$acquisitionDate[1]
script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
ras <- GetImage(aoi = aoi, time_range = day, script = script_file, 
    collection = "sentinel-2-l2a", format = "image/tiff", mosaicking_order = "leastCC", 
    resolution = 10, mask = TRUE, buffer = 100, client = OAuthClient)
ras
ras[ras < 0] <- 0
terra::plot(ras, main = paste("Central Park NDVI on", day), cex.main = 0.75,
    col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))


## ----label="retrieve RGB image", fig.cap="Central Park image as PNG file", fig.width=5, fig.height=3, warning=FALSE----
bbox <- as.numeric(sf::st_bbox(aoi))
script_text <- paste(readLines(system.file("scripts", "TrueColorS2L2A.js", 
    package = "CDSE")), collapse = "\n")
cat(c(readLines(system.file("scripts", "TrueColorS2L2A.js", package = "CDSE"), n = 15), 
    "..."), sep = "\n")
png <- tempfile("img", fileext = ".png")
GetImage(bbox = bbox, time_range = day, script = script_text, 
    collection = "sentinel-2-l2a", file = png, format = "image/png", 
    mosaicking_order = "leastCC", pixels = c(600, 950), client = OAuthClient)
terra::plotRGB(terra::rast(png))

## ----label="retrieve images in parallel", fig.cap="Central Park monthly NDVI"----
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2022-01-01", to = "2022-12-31",
                        collection = "sentinel-2-l2a", with_geometry = TRUE, 
                        filter = "eo:cloud_cover < 5", client = OAuthClient)
# Get the day with the minimal cloud cover for every month -----------------------------
tmp1 <- images[, c("tileCloudCover", "acquisitionDate")]
tmp1$month <- lubridate::month(images$acquisitionDate)
agg1 <- stats::aggregate(tileCloudCover ~ month, data = tmp1, FUN = min)
tmp2 <- merge.data.frame(agg1, tmp1, by = c("month", "tileCloudCover"), sort = FALSE)
# in case of ties, get an arbitrary date (here the smallest acquisitionDate, 
# could also be the biggest)
agg2 <- stats::aggregate(acquisitionDate ~ month, data = tmp2, FUN = min)
monthly <- merge.data.frame(agg2, tmp2, by = c("acquisitionDate", "month"), sort = FALSE)
days <- monthly$acquisitionDate
# Retrieve images in parallel ----------------------------------------------------------
script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
tmp_folder <- tempfile("dir")
if (!dir.exists(tmp_folder)) dir.create(tmp_folder)
cl <- parallel::makeCluster(4)
ans <- parallel::clusterExport(cl, list("tmp_folder"), envir = environment())
ans <- parallel::clusterEvalQ(cl, {library(CDSE)})
lstRast <- parallel::parLapply(cl, days, fun = function(x, ...) {
    GetImageByTimerange(x, file = sprintf("%s/img_%s.tiff", tmp_folder, x), ...)},
    aoi = aoi, collection = "sentinel-2-l2a", script = script_file,
    format = "image/tiff", mosaicking_order = "mostRecent", resolution = 10,
    buffer = 0, mask = TRUE, client = OAuthClient)
parallel::stopCluster(cl)
# Plot the images ----------------------------------------------------------------------
par(mfrow = c(3, 4))
ans <- sapply(seq_along(days), FUN = function(i) {
    ras <- terra::rast(lstRast[[i]])
    day <- days[i]
    ras[ras < 0] <- 0
    terra::plot(ras, main = paste("Central Park NDVI on", day), range = c(0, 1),
        cex.main = 0.7, pax = list(cex.axis = 0.5), plg = list(cex = 0.5),
        col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))
    })

## ----label="simple statistics"------------------------------------------------

dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
script_file <- system.file("scripts", "NDVI_CLOUDS_STAT.js", package = "CDSE")
daily_stats <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
    collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC",
    resolution = 100, aggregation_period = 1, client = OAuthClient)
weekly_stats <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
    collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC",
    resolution = 100, aggregation_period = 7, client = OAuthClient)
weekly_stats_extended <- GetStatistics(aoi = aoi, 
    time_range = c("2023-07-01", "2023-07-31"), collection = "sentinel-2-l2a", 
    script = script_file, mosaicking_order = "leastCC", resolution = 100, 
    aggregation_period = 1, aggregation_unit = "w", lastIntervalBehavior = "EXTEND", 
    client = OAuthClient)
daily_stats
weekly_stats
weekly_stats_extended

## ----label="simple statistics and percentiles"--------------------------------
daily_stats <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
    collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC",
    resolution = 100, aggregation_period = 1, percentiles = c(25, 50, 75), 
    client = OAuthClient)
head(daily_stats, n = 10)
weekly_stats <- GetStatistics(aoi = aoi, time_range = c("2023-07-01", "2023-07-31"),
    collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC",
    resolution = 100, aggregation_period = 7, percentiles = seq(10, 90, by = 10), 
    client = OAuthClient)
head(weekly_stats, n = 10)

## ----label="series of statistics"---------------------------------------------
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
ndvi_script <- paste(MakeEvalScript(
    list(
        bands = c("N", "R"),
        formula = "(N - R)/(N + R)",
        long_name = "Normalized Difference Vegetation Index",
        platforms = "Sentinel-2"
    )
), collapse = "\n")
seasons <- SeasonalTimerange(from = "2020-06-01", to = "2023-08-31")
lst_stats <- lapply(seasons, GetStatisticsByTimerange, aoi = aoi, 
    collection = "sentinel-2-l2a", script = ndvi_script, mosaicking_order = "leastCC", 
    resolution = 100, aggregation_period = 7L, client = OAuthClient)
weekly_stats <- do.call(rbind, lst_stats)
weekly_stats <- weekly_stats[order(weekly_stats$from), ]
row.names(weekly_stats) <- NULL
head(weekly_stats, n = 5)

