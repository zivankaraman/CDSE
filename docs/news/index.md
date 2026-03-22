# Changelog

## CDSE 0.3.2

- Added function
  [`SetPathAPI()`](https://zivankaraman.github.io/CDSE/reference/SetPathAPI.md)
  that enables switching between the new and old API path structures
  (see this
  [announcement](https://dataspace.copernicus.eu/news/2026-3-9-api-path-structure-updates-sentinel-hub-services))

## CDSE 0.3.1

- Adapted
  [`MakeEvalScript()`](https://zivankaraman.github.io/CDSE/reference/MakeEvalScript.md)
  function to handle Landsat-8/9 data

## CDSE 0.3.0

CRAN release: 2025-09-02

- Adapted
  [`GetCollections()`](https://zivankaraman.github.io/CDSE/reference/GetCollections.md)
  function to handle multi-instrument collections like ‘Sentinel 3
  Synergy L2’
- Added
  [`MakeEvalScript()`](https://zivankaraman.github.io/CDSE/reference/MakeEvalScript.md)
  function that creates evalscript from spectral indices defined in the
  `rsi` package
- Expanded documentation on evalscripts
- Requests that fail are automatically retried in case of error 429
  (“too many requests”, often used for rate limiting) or 503 (“service
  unavailable’)
- Fixed internal function `CheckBbox()`

## CDSE 0.2.1

CRAN release: 2024-08-31

- Created package website with pkgdown
- Added aggregation period units choice to statistical values
- Fixed updated external URLs
- Improved documentation

## CDSE 0.2.0

CRAN release: 2024-04-30

- Added Statistical API wrapper
- Added CSL2 filter to image catalog search
- Added seasonal filtering for image catalog
- Improved documentation

## CDSE 0.1.0

CRAN release: 2023-12-05

- Initial CRAN submission.
