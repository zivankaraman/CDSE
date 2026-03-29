# Load required libraries ----------------------------------------------------
library(sf)
library(terra)
library(CDSE)

# Authentication -------------------------------------------------------------
id <- Sys.getenv("CDSE_ID")
secret <- Sys.getenv("CDSE_SECRET")
OAuthClient <- GetOAuthClient(id = id, secret = secret)

# Define Area of Interest ----------------------------------------------------
dsn <- system.file("extdata", "vincennes.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)

# Select overlapping day -----------------------------------------------------
day <- "2022-08-13"

# Sentinel-2 image ------------------------------------------------------
s2_script <- paste(MakeEvalScript("NDVI", constellation = "sentinel-2"), collapse = "\n")
ras_s2 <- GetImage(aoi = aoi, time_range = day, script = s2_script,
                collection = "sentinel-2-l2a", format = "image/tiff", mosaicking_order = "leastCC",
                resolution = 3, mask = TRUE, buffer = 100, client = OAuthClient)
ras_s2[ras_s2 < 0] <- 0
plot(ras_s2, main = "Sentinel-2", range = c(0, 0.95),
     col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))

# Landsat 8 image -------------------------------------------------------
ls_script <- paste(MakeEvalScript("NDVI", constellation = "landsat"), collapse = "\n")
ras_ls <- GetImage(aoi = aoi, time_range = day, script = ls_script,
                   collection = "landsat-ot-l1", format = "image/tiff", mosaicking_order = "leastCC",
                   resolution = 3, mask = TRUE, buffer = 100, client = OAuthClient)
ras_ls[ras_ls < 0] <- 0
plot(ras_ls, main = "Landsat-8", range = c(0, 0.95),
     col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))
