# CDSE 0.3.O (2025-08-05)

-   Adapted `GetCollections` to handle multi-instrument collections like 'Sentinel 3 Synergy L2'
-   Added `MakeEvalScript` function that creates evalscript from spectral indices defined in the `rsi` package
-   Expanded documentation on evalscripts
-   Requests that fail are automatically retried in case of error 429 ("too many requests", often used for rate limiting) or 503 ("service unavailable')
-   Fixed internal function `CheckBbox`

# CDSE 0.2.1 (2024-08-16)

-   Created package website with pkgdown
-   Added aggregation period units choice to statistical values
-   Fixed updated external URLs
-   Improved documentation

# CDSE 0.2.0 (2024-04-30)

-   Added Statistical API wrapper
-   Added CSL2 filter to image catalog search
-   Added seasonal filtering for image catalog
-   Improved documentation

# CDSE 0.1.0 (2023-12-05)

-   Initial CRAN submission.
