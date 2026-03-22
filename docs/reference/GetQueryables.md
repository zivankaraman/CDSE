# Get CQL2 parameters for a collection

Returns a list of variable terms that can be used in CQL2 expressions to
filter the collection catalog search.

## Usage

``` r
GetQueryables(
  collection,
  as_data_frame = TRUE,
  client,
  token,
  url = getOption("CDSE.catalog_url")
)
```

## Source

<https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/ApiReference.html#tag/catalog_collections/operation/getCatalogCollectionQueryables>

## Arguments

- collection:

  character indicating the collection for which the parameters are
  queried. Must be one of the collections returned by `GetCollections`.

- as_data_frame:

  logical indicating if the result should be returned as data frame.
  Default: TRUE

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

A `list` or a `data.frame`.

## Details

If no parameters found, a `NULL` value or 0-row `data.frame` is
returned.

## See also

[`GetCollections`](https://zivankaraman.github.io/CDSE/reference/GetCollections.md),
[`SearchCatalog`](https://zivankaraman.github.io/CDSE/reference/SearchCatalog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
GetQueryables("sentinel-2-l2a", client = OAuthClient)
} # }
```
