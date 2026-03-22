# Search collection for available images

Searches the specified collection for available images in the given time
interval and intersecting with the bounding box or the area of interest.

## Usage

``` r
SearchCatalog(
  aoi,
  bbox,
  from,
  to,
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

- aoi:

  sf or sfc object, typically a (multi)polygon, describing the Area of
  Interest.

- bbox:

  numeric vector of four elements describing the bounding box of
  interest. Specify with a coordinate pair on two (opposite) vertices of
  the bounding box rectangle. Coordinates need to be in longitude,
  latitude.

  Only one of either `aoi` or `bbox` may be specified.

- from:

  start of the time interval to search.

- to:

  end of the time interval to search.

  `from` and `to` can be either Date or character that can be converted
  to date by `as.Date`.

  Open interval (one side only) can be obtained by providing the `NA` or
  `NULL` value for the corresponding argument.

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

## See also

[`GetCollections`](https://zivankaraman.github.io/CDSE/reference/GetCollections.md),
[`GetQueryables`](https://zivankaraman.github.io/CDSE/reference/GetQueryables.md),
[`GetImage`](https://zivankaraman.github.io/CDSE/reference/GetImage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dsn <- system.file("extdata", "luxembourg.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
images <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31",
          collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
images_cloudless <- SearchCatalog(aoi = aoi, from = "2023-07-01", to = "2023-07-31",
          filter = "eo:cloud_cover < 5",
          collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
} # }
```
