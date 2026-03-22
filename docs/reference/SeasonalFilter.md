# Filter image catalog for seasonal images

Filters image catalog entries that fall in the season of interest -
dates between `from` day/month and `to` day/month for all years in the
`from` - `to` time range.

## Usage

``` r
SeasonalFilter(catalog, from, to)
```

## Arguments

- catalog:

  `data.frame` or `sf` object as the one produced by a call to
  `SearchCatalog`

- from:

  start of the season of interest.

- to:

  end of the season of interest.

  The `from` and `to` arguments can be either Date or character that can
  be converted to date by `as.Date`. Open intervals are not allowed
  (both `from` and `to` must be valid dates).

## Value

A `data.frame` or a `sf` object, depending on the type of the input.

## See also

[`SearchCatalog`](https://zivankaraman.github.io/CDSE/reference/SearchCatalog.md),
[`SeasonalTimerange`](https://zivankaraman.github.io/CDSE/reference/SeasonalTimerange.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dsn <- system.file("extdata", "centralpark.geojson", package = "CDSE")
aoi <- sf::read_sf(dsn, as_tibble = FALSE)
all_images <- SearchCatalog(aoi = aoi, from = "2021-06-01", to = "2023-08-31",
    collection = "sentinel-2-l2a", with_geometry = TRUE, client = OAuthClient)
sesonal_images <- SeasonalFilter(all_images, from = "2021-06-01", to = "2023-08-31")
} # }
```
