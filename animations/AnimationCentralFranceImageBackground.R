## Monthly average NDVI for some fields in Central France in 2022
## Images with satellite image in background

# Load required libraries --------------------------------------------------------------
library(sf)
library(terra)
library(fields)
library(magick)
library(CDSE)

# Authenticate -------------------------------------------------------------------------
id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
OAuthClient <- GetOAuthClient(id = id, secret = secret)

# Get AOI ---------------------------------------------------------------------------
dsn <- "https://github.com/zivankaraman/CDSE/raw/refs/heads/master/animations/fieldBoundaries.rds"
fields <- readRDS(url(dsn, open = "rb"))

# Get background images ----------------------------------------------------------------

# Evalscript for background ------------------------------------------------------------
script_file <- system.file("scripts", "TrueColorS2L2A.js", package = "CDSE")

# Catalog ------------------------------------------------------------------------------
images <- SearchCatalog(bbox = st_bbox(fields), from = "2022-01-01", to = "2022-12-31",
                        collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
images <- subset(images, areaCoverage == 100)

# Get the day with the minimal cloud cover for every month -----------------------------
tmp1 <- images[, c("tileCloudCover", "acquisitionDate")]
tmp1$month <- lubridate::month(images$acquisitionDate)
agg1 <- stats::aggregate(tileCloudCover ~ month, data = tmp1, FUN = min)
tmp2 <- merge.data.frame(agg1, tmp1, by = c("month", "tileCloudCover"), sort = FALSE)
# in case of ties, get an arbitrary date (here the smallest acquisitionDate, could also be the biggest)
agg2 <- stats::aggregate(acquisitionDate ~ month, data = tmp2, FUN = min)
monthly <- merge.data.frame(agg2, tmp2, by = c("acquisitionDate", "month"), sort = FALSE)
# fix December, the default image is cloudy
december <- subset(images, acquisitionDate == "2022-12-11", c(acquisitionDate, tileCloudCover))
december$month <- 12
monthly[12, ] <- december[, c("acquisitionDate", "month", "tileCloudCover", "geometry")]

# Retrieve the best monthly NDVI images ------------------------------------------------
size <- 1920L       # max dimension (number of pixels) of the output image
pix <- 1920L / 4L   # raster resolution (this is largely sufficient)
bb <- st_bbox(fields)
lstBackground <- lapply(1:12, FUN = function(i) {
    day <- monthly$acquisitionDate[i]
    ras <- GetImage(bbox = bb, time_range = day, script = script_file,
                    collection = "sentinel-2-l2a", format = "image/tiff",
                    mosaicking_order = "leastCC", pixels = pix,
                    buffer = 2000, client = OAuthClient)
    # use 2km buffer around the AOI
    wrap(ras)
})
# Determine the aspect ratio and dimensions for the output image
ras <- unwrap(lstBackground[[1]])
ex <- as.vector(terra::ext(ras))
bl <- st_sfc(st_point(c(ex[1], ex[3])), crs = 4326)     # bottom left
tl <- st_sfc(st_point(c(ex[1], ex[4])), crs = 4326)     # top left
br <- st_sfc(st_point(c(ex[2], ex[3])), crs = 4326)     # bottom right
tr <- st_sfc(st_point(c(ex[2], ex[4])), crs = 4326)     # top right
width <- as.numeric(st_distance(tl, tr))
height <- as.numeric(st_distance(tl, bl))
big <- max(width, height)
w <- as.integer(size * width / big)
h <- as.integer(size * height / big)

# Get monthly statistics ---------------------------------------------------------------

# Evalscript for statistics ------------------------------------------------------------
evalscript <- '
//VERSION=3
function setup() {
  return {
    input: [{
      bands: [
        "B04",
        "B08",
        "SCL",
        "dataMask"
      ]
    }],
    mosaicking: "ORBIT",
    output: [
      {
        id: "data",
        bands: ["monthly_max_ndvi"]
      },
      {
        id: "dataMask",
        bands: 1
      }]
  }
}

function evaluatePixel(samples) {
    var max = 0;
    var hasData = 0;
    for (var i=0;i<samples.length;i++) {
      if (samples[i].dataMask == 1 && samples[i].B04+samples[i].B08 != 0 ){
        hasData = 1
        var ndvi = (samples[i].B08 - samples[i].B04)/(samples[i].B08 + samples[i].B04);
        max = ndvi > max ? ndvi:max;
      }
    }

    return {
        data: [max],
        dataMask: [hasData]
    }
}
'

# Prepare data -------------------------------------------------------------------------
yr <- 2022
period <- c(10000 * yr + 101, 10000 * yr + 1231)
# Convert boundaries into a list of sf objects, each containing a single feature
list_of_sf_objects <- lapply(1:nrow(fields), function(i) {
    # Subset the ith feature and retain its geometry and attributes
    st_as_sf(fields[i, ])
})
# Now 'list_of_sf_objects' is a list where each element is a single-feature sf object
N <- length(list_of_sf_objects)
stats <- vector(mode = "list", length = N)

# Retrieve statistics ------------------------------------------------------------------
for (i in 1:N) {
    msg <- sprintf("\rprocessing field %3d/%d", i, N)
    cat(msg, sep = "")
    stats[[i]] <- GetStatistics(aoi = list_of_sf_objects[[i]],
                                time_range = period, collection = "sentinel-2-l2a", script = evalscript,
                                aggregation_period = 1, aggregation_unit =  "m", lastIntervalBehavior = "skip",
                                percentiles = c(25, 50, 75),
                                mosaicking_order = "leastCC", resolution = 20, client = OAuthClient)
}

# Post process retrieved statistics ----------------------------------------------------
# update id
for (i in 1:length(stats)) {
    stats[[i]]$id <- fields$id_parcel[i]
}
# convert list to data.frame
stats_df <- do.call(rbind, stats)
# merge fields boundaries with statistics
dat <- merge(fields, stats_df, by.x = "id_parcel", by.y = "id")
months <- levels(factor(dat$from))

# Produce monthly plots ----------------------------------------------------------------
# define legend
subTitle <- paste("        Average monthly NDVI values per field",
                  "        Barley, maize, rapeseed, sunflower, and wheat fields in Central France, 2022",
                  sep = "\n")
farben <- list(axis = "azure", border = "royalblue4", main = "lightcyan", sub = "gray100")
folder <- tempfile("dir")
if (!dir.exists(folder)) dir.create(folder)
for (i in 1:12) {
    m <- months[i]
    tmp <- subset(dat, from == m)
    fp <- file.path(folder, sprintf("month%2.2d.png", i))
    png(fp, pointsize = 40, height = h, width = w, bg = "transparent")
    par(cex.axis = 0.6, col.axis = farben$axis)
    ras <- unwrap(lstBackground[[i]])
    plotRGB(ras)
    plot(tmp["median"],
         breaks = seq(0, 1, by = 0.01),
         pal = colorRampPalette(c("darkred", "yellow", "darkgreen"))(100),
         border = farben$border, lwd = 2,
         key.pos = NULL, main = "", reset = FALSE, add = TRUE)
    title(main = month.name[i], cex.main = 1, line = 1, col.main = farben$main)
    mtext(subTitle, side = 1, adj = 0, line = 3, col = farben$sub, cex = 0.6, font = 4)
    leg <- fields::setupLegend(legend.shrink = 0.5)
    fields::addLegend(leg, col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(100),
                      zlim = c(0,1))
    dev.off()
}

# Combine images into animated GIF -----------------------------------------------------
# list file names and read in
imgs <- list.files(folder, full.names = TRUE)
img_list <- lapply(imgs, magick::image_read)
# join the images together
img_joined <- magick::image_join(img_list)
# animate at 1 frame per second
img_animated <- magick::image_animate(img_joined, fps = 1)
# view animated image
# img_animated
# save to disk
gif <- paste0(folder, "/Crops.gif")
magick::image_write(image = img_animated, path = gif)
message(sprintf("animation written to file '%s'", gif))
