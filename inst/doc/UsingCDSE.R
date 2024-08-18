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
summer_images

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
cloudless_images <- SearchCatalog(aoi = aoi, from = "2023-01-01", to = "2023-12-31",
    collection = "sentinel-2-l2a", with_geometry = TRUE, filter = "eo:cloud_cover < 0.8", 
    client = OAuthClient)
script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
days <- rev(cloudless_images$acquisitionDate)
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
    collection = "sentinel-2-l2a", script = script_file,mosaicking_order = "leastCC",
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
    collection = "sentinel-2-l2a", script = script_file,mosaicking_order = "leastCC",
    resolution = 100, aggregation_period = 7, percentiles = seq(10, 90, by = 10), 
    client = OAuthClient)
head(weekly_stats, n = 10)

## ----label="series of statistics"---------------------------------------------
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
script_file <- system.file("scripts", "NDVI_dataMask_float32.js", package = "CDSE")
seasons <- SeasonalTimerange(from = "2020-06-01", to = "2023-08-31")
lst_stats <- lapply(seasons, GetStatisticsByTimerange, aoi = aoi, 
    collection = "sentinel-2-l2a", script = script_file, mosaicking_order = "leastCC", 
    resolution = 100, aggregation_period = 7L, client = OAuthClient)
weekly_stats <- do.call(rbind, lst_stats)
weekly_stats <- weekly_stats[order(weekly_stats$from), ]
row.names(weekly_stats) <- NULL
head(weekly_stats)

