# Load required libraries --------------------------------------------------------------

library(magick)
library(sf)
library(terra)
library(CDSE)


# Authentication ------------------------------------------------------------------------

id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
OAuthClient <- GetOAuthClient(id = id, secret = secret)


# Search catalog to select images ------------------------------------------------------

dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2022-01-01", to = "2022-12-31",
                        collection = "sentinel-2-l2a", with_geometry = FALSE,
                        client = OAuthClient)
summary(images$tileCloudCover)


# Get the day with the minimal cloud cover for every month -----------------------------

tmp1 <- images[, c("tileCloudCover", "acquisitionDate")]
tmp1$month <- lubridate::month(images$acquisitionDate)
agg1 <- stats::aggregate(tileCloudCover ~ month, data = tmp1, FUN = min)
tmp2 <- merge.data.frame(agg1, tmp1, by = c("month", "tileCloudCover"), sort = FALSE)
# in case of ties, get an arbitrary date (here the smallest acquisitionDate, could also be the biggest)
agg2 <- stats::aggregate(acquisitionDate ~ month, data = tmp2, FUN = min)
monthly <- merge.data.frame(agg2, tmp2, by = c("acquisitionDate", "month"), sort = FALSE)


# Retrieve the best monthly NDVI images ------------------------------------------------

script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
folder <- tempfile("dir")
if (!dir.exists(folder)) dir.create(folder)
# define bg/fg colors
fg <- "#DDDDDD"
bg <- "#444444"
for (i in 1:12) {
    day <- monthly$acquisitionDate[i]
    ras <- GetImage(aoi = aoi, time_range = day, script = script_file,
                       collection = "sentinel-2-l2a", format = "image/tiff",
                       mosaicking_order = "leastCC", resolution = 10,
                       mask = TRUE, buffer = 100, client = OAuthClient)
    ras[ras < 0] <- 0
    fp <- file.path(folder, sprintf("month%2.2d.png", i))
    png(fp)
    par(col = fg, col.main = fg, bg = bg)
    terra::plot(ras, main = paste("NYC Central Park NDVI\n", lubridate::month(day, label = TRUE, abbr = FALSE, locale = "C"), 2022),
                range = 0:1, col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99),
                axes = FALSE, plg = list(size = 0.5, cex = 0.75), , cex.main = 0.8)
    terra::north(xy = "topleft", type = 2, col = "white", xpd = TRUE, cex = 0.8, label = "N", adj = c(0.55, 0.2))
    terra::sbar(d = 1, xy = "bottomright", type = "bar", below = "m", labels = c(0, 500, 1000),
                lonlat = NULL, cex = 0.5, col = "white", divs = 4)
    dev.off()
}


# Combine images into animated GIF -----------------------------------------------------

# list file names and read in
imgs <- list.files(folder, full.names = TRUE)
img_list <- lapply(imgs, magick::image_read)

# join the images together
img_joined <- magick::image_join(img_list)

# animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 2)

# view animated image
img_animated

# save to disk
magick::image_write(image = img_animated, path = "CentralPark.gif")

