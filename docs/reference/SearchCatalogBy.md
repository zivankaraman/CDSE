# Search collection for available images (vectorization ready)

These functions search the specified collection for available images
using the parameters provided. They are simple wrappers around the
'SearchCatalog' function with arguments organized in a way that
facilitates calling the function in a vectorized manner (using 'lapply'
or similar function) and thus potentially also the parallelization. The
'from' and 'to' arguments are combined into a single argument
'time_range'.

## Usage

``` r
SearchCatalogByTimerange(
  time_range,
  aoi,
  bbox,
  collection,
  as_data_frame = TRUE,
  with_geometry = TRUE,
  filter = NULL,
  client,
  token,
  url = getOption("CDSE.catalog_url")
)

SearchCatalogByAOI(
  aoi,
  time_range,
  collection,
  as_data_frame = TRUE,
  with_geometry = TRUE,
  filter = NULL,
  client,
  token,
  url = getOption("CDSE.catalog_url")
)

SearchCatalogByBbox(
  bbox,
  time_range,
  collection,
  as_data_frame = TRUE,
  with_geometry = TRUE,
  filter = NULL,
  client,
  token,
  url = getOption("CDSE.catalog_url")
)
```

## Source

<https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Catalog.html>

## Arguments

- time_range:

  scalar or vector (Date or character that can be converted to date)
  defining the time interval. Open interval (one side only) can be
  obtained by providing the `NA` or `NULL` value for the corresponding
  argument.

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

- as_data_frame:

  logical indicating if the result should be returned as data frame.
  Default: TRUE

- with_geometry:

  logical indicating if the granule geometries should be included in the
  data.frame. Default: TRUE

- filter:

  character, CQL2 text filter. Use the function `GetQueryables` to find
  out which filters can bu used with the collection. Default: NULL (no
  filtering)

- client:

  OAuth client object to use for authentication.

- token:

  OAuth token character string to use for authentication.

  Exactly one of either `client` or `token` must be specified. It is
  recommended to use `client`.

- url:

  character indicating the STAC catalog search endpoint. Default:
  Copernicus Data Space Ecosystem STAC endpoint

## Value

A `list`, `data.frame` or a `sf` object.

## Details

If no images found, a `NULL` value is returned.

`SearchCatalogByTimerange` is arranged for vectorization on time_range
(time_range is the first argument).

`SearchCatalogByAOI` is arranged for vectorization on aoi (aoi is the
first argument).

`SearchCatalogByBbox` is arranged for vectorization on bbox (bbox is the
first argument).

## See also

[`SearchCatalog`](https://zivankaraman.github.io/CDSE/reference/SearchCatalog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
seasons <- SeasonalTimerange(from = "2020-06-01", to = "2023-08-31")
lst_images_cloudless <- lapply(seasons, SearchCatalogByTimerange, aoi = aoi,
    collection = "sentinel-2-l2a", with_geometry = FALSE,
    filter = "eo:cloud_cover < 5", client = OAuthClient)
images_cloudless <- do.call(rbind, lst_images_cloudless)
images_cloudless <- images_cloudless[rev(order(images_cloudless$acquisitionDate)), ]
row.names(images_cloudless) <- NULL
head(images_cloudless[, 1:5])
} # }
```
