# Create seasonal time range

Creates list of seasonal filters (one per year) for the season of
interest - dates between `from` day/month and `to` day/month for all
years in the `from` - `to` time range.

## Usage

``` r
SeasonalTimerange(from, to)
```

## Arguments

- from:

  start of the season of interest.

- to:

  end of the season of interest.

  The `from` and `to` arguments can be either Date or character that can
  be converted to date by `as.Date`. Open intervals are not allowed
  (both `from` and `to` must be valid dates).

## Value

A list of time ranges defining the season of interest for each year.

## Examples

``` r
if (FALSE) { # \dontrun{
seasons <- SeasonalTimerange(from = "2020-05-01", to = "2023-09-30")
seasons <- SeasonalTimerange(from = "2019-11-01", to = "2023-03-30")
} # }
```
