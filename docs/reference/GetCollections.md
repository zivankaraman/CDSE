# List available collections

Retrieves the list of available imagery collections.

## Usage

``` r
GetCollections(as_data_frame = TRUE, url = getOption("CDSE.catalog_url"))
```

## Source

<https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Catalog.html>

## Arguments

- as_data_frame:

  logical indicating if the result should be returned as data frame.
  Default: TRUE

- url:

  character indicating the STAC catalog search endpoint. Default:
  Copernicus Data Space Ecosystem STAC endpoint

## Value

A `list` or a `data.frame` of all available imagery collections and
their attributes.

## Details

This function doesn't require authentication.

## See also

[`GetImage`](https://zivankaraman.github.io/CDSE/reference/GetImage.md),
[`SearchCatalog`](https://zivankaraman.github.io/CDSE/reference/SearchCatalog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
GetCollections(as_data_frame = TRUE)
} # }
```
