# Get image from the archive (vectorization ready)

These functions retrieve the image for the area of interest using the
parameters provided. They are simple wrappers around the 'GetImage'
function with arguments organized in a way that facilitates calling the
function in a vectorized manner (using 'lapply' or similar function) and
thus potentially also the parallelization.

## Usage

``` r
GetImageByTimerange(
  time_range,
  aoi,
  bbox,
  collection,
  script,
  file = NULL,
  format = c("image/tiff", "image/png", "image/jpeg"),
  mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
  pixels,
  resolution,
  buffer = 0,
  mask = FALSE,
  client,
  token,
  url = getOption("CDSE.process_url")
)

GetImageByAOI(
  aoi,
  time_range,
  collection,
  script,
  file = NULL,
  format = c("image/tiff", "image/png", "image/jpeg"),
  mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
  pixels,
  resolution,
  buffer = 0,
  mask = FALSE,
  client,
  token,
  url = getOption("CDSE.process_url")
)

GetImageByBbox(
  bbox,
  time_range,
  collection,
  script,
  file = NULL,
  format = c("image/tiff", "image/png", "image/jpeg"),
  mosaicking_order = c("mostRecent", "leastRecent", "leastCC"),
  pixels,
  resolution,
  buffer = 0,
  mask = FALSE,
  client,
  token,
  url = getOption("CDSE.process_url")
)
```

## Source

<https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Process.html>

## Arguments

- time_range:

  scalar or vector (Date or character that can be converted to date)
  defining the time interval.

- aoi:

  sf or sfc object, typically a (multi)polygon, describing the Area of
  Interest.

- bbox:

  numeric vector of four elements describing the bounding box of
  interest. Specify with a coordinate pair on two (opposite) vertices of
  the bounding box rectangle. Coordinates need to be in longitude,
  latitude.

  Only one of either `aoi` or `bbox` may be specified.

- collection:

  character indicating which collection to search. Must be one of the
  collections returned by `GetCollections`.

- script:

  a length one character string containing the evaluation script or the
  name of the file containing the script.

- file:

  name of the file to save the image. If NULL, a `SpatRaster` object is
  returned. Default: NULL

- format:

  character indicating the output file format. Must be one of
  "image/tiff", "image/png", or "image/jpeg". Default: "image/tiff"

- mosaicking_order:

  character indicating the order in which tiles are overlapped from
  which the output result is mosaicked. Must be one of "mostRecent",
  "leastRecent", or "leastCC". Default: "mostRecent"

- pixels:

  integer scalar or length-two vector indicating the request image width
  and height. Values must be integers between 1 and 2500.

- resolution:

  numeric scalar or length-two vector indicating the spatial resolution
  of the request image in horizontal and vertical direction (in meters).

  Only one of the arguments "pixels" or "resolution" must be set at the
  same time. If the argument "pixels" or "resolution" is scalar, the
  same value is used for horizontal and vertical direction (width and
  height).

- buffer:

  numeric, width of the buffer to retrieve the image of enlarged area.
  Default: 0

- mask:

  logical indicating if the image should contain only pixels within Area
  of Interest. Default: FALSE

- client:

  OAuth client object to use for authentication.

- token:

  OAuth token character string to use for authentication.

  Exactly one of either `client` or `token` must be specified. It is
  recommended to use `client`.

- url:

  character indicating the process endpoint. Default: Copernicus Data
  Space Ecosystem process endpoint

## Value

`SpatRaster` object (from the package `terra`) of the requested image
(if `file` is `NULL`), or the (invisible) name of the file created.

## Details

If `aoi` argument is provided, the result is returned in the same
coordinate reference system.

`GetImageByTimerange` is arranged for vectorization on time_range
(time_range is the first argument).

`GetImageByAOI` is arranged for vectorization on aoi (aoi is the first
argument).

`GetImageByBbox` is arranged for vectorization on bbox (bbox is the
first argument).

## See also

[`GetImage`](https://zivankaraman.github.io/CDSE/reference/GetImage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
cloudless_images <- SearchCatalog(aoi = aoi, from = "2023-01-01", to = "2023-12-31",
                    collection = "sentinel-2-l2a", with_geometry = TRUE,
                    filter = "eo:cloud_cover < 0.8", client = OAuthClient)
script_file <- system.file("scripts", "NDVI_float32.js", package = "CDSE")
days <- rev(cloudless_images$acquisitionDate)
lstRast <- lapply(days, GetImageByTimerange, aoi = aoi, collection = "sentinel-2-l2a",
    script = script_file, file = NULL, format = "image/tiff", mosaicking_order = "mostRecent",
    resolution = 10, buffer = 0, mask = TRUE, client = OAuthClient,
    url = getOption("CDSE.process_url"))
par(mfrow = c(3, 4))
sapply(seq_along(days), FUN = function(i) {
     ras <- lstRast[[i]]
     day <- days[i]
     ras[ras < 0] <- 0
     terra::plot(ras, main = paste("Central Park NDVI on", day), range = c(0, 1),
            cex.main = 0.7, pax = list(cex.axis = 0.5), plg = list(cex = 0.5),
            col = colorRampPalette(c("darkred", "yellow", "darkgreen"))(99))
     })
} # }
```
