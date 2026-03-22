# Set API Path Structure

Switch between the legacy and the new API path structure for Sentinel
Hub services on the Copernicus Data Space Ecosystem.

## Usage

``` r
SetPathAPI(new = FALSE)
```

## Arguments

- new:

  logical, indicating whether to use the new API path structure. Default
  is `FALSE` (uses legacy format).

## Value

No return value, called for side effects (sets the API endpoints).

## Details

As of March 2026, the Sentinel Hub APIs on the Copernicus Data Space
Ecosystem support an additional format alongside the legacy paths.

- Legacy format: `/api/<version>/<service>`

- New format: `/<service>/<version>`

Setting `new = TRUE` switches the URLs for process and statistics API
endpoints to the new format. For more details, see
<https://dataspace.copernicus.eu/news/2026-3-9-api-path-structure-updates-sentinel-hub-services>.

## Examples

``` r
if (FALSE) { # \dontrun{
SetPathAPI(new = TRUE)
} # }
```
