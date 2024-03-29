# CDSE

'Copernicus Data Space Ecosystem' API Wrapper

## Description

This package provides the interface to the ['Copernicus Data Space Ecosystem' API](https://dataspace.copernicus.eu/analyse/apis), mainly for searching the catalogue of available data from Copernicus Sentinel missions and obtaining the images for just the area of interest based on selected spectral bands. The package uses the 'Sentinel Hub' RESTful [API interface](https://dataspace.copernicus.eu/analyse/apis/sentinel-hub) to access various satellite imagery archives. It allows you to access raw satellite data, rendered images, statistical analysis, and other features.

## Installation

### Stable version

You can install the current stable version of `CDSE` from [CRAN](https://cran.r-project.org/package=CDSE) via:

``` r
install.packages("CDSE")
```

Windows and MacOS binary packages are available from here.

### Development version

You can install the latest development version of `CDSE` from GitHub with:

``` r
require(remotes)
install_github("zivankaraman/CDSE")
```

## License

This package is released under [AGPLv3](https://cran.r-project.org/web/licenses/AGPL-3).
