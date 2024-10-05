## Monthly average NDVI for some fields in Central France in 2022
## Images with black background

# Load required libraries --------------------------------------------------------------
library(sf)
library(magick)
library(CDSE)

# Authenticate -------------------------------------------------------------------------
id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
OAuthClient <- GetOAuthClient(id = id, secret = secret)

# Get AOI ------------------------------------------------------------------------------
dsn <- "https://github.com/zivankaraman/CDSE/raw/refs/heads/master/animations/fieldBoundaries.rds"
fields <- readRDS(url(dsn, open = "rb"))

# Evalscript ---------------------------------------------------------------------------
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
# define bg/fg colors
fg <- "#DDDDDD"
bg <- "#444444"
# define legend
subTitle <- paste("Average monthly NDVI values per field",
                  "Barley, maize, rapeseed, sunflower, and wheat fields in Central France, 2022",
                  sep = "\n")
folder <- tempfile("dir")
if (!dir.exists(folder)) dir.create(folder)
size <- 1920
for (i in 1:12) {
    m <- months[i]
    tmp <- subset(dat, from == m)
    fp <- file.path(folder, sprintf("month%2.2d.png", i))
    png(fp, pointsize = 40, height = size, width = size)
    par(col = fg, col.main = fg, col.axis = fg, bg = bg, las = 1, font = 2)
    plot(tmp["median"],
         breaks = seq(0, 1, by = 0.01),
         pal = colorRampPalette(c("darkred", "yellow", "darkgreen"))(100),
         border = "cyan", lwd = 2, cex.axis = 0.6,
         key.pos = 4, main = "", reset = FALSE)
    mtext(subTitle, side = 1, adj = 0, line = -1, col = fg, cex = 0.5, font = 4)
    title(main = month.name[i], cex.main = 1, line = -1)
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
