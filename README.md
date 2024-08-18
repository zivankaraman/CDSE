# CDSE <a href="https://zivankaraman.github.io/CDSE/"><img src="man/figures/logo.png" alt="CDSE website" align="right" height="140"/></a>

**'Copernicus Data Space Ecosystem' API Wrapper**

This package provides the interface to the ['Copernicus Data Space Ecosystem' API](https://dataspace.copernicus.eu/analyse/apis), mainly for searching the catalogue of available data from Copernicus Sentinel missions and obtaining the images for just the area of interest based on selected spectral bands. The package uses the 'Sentinel Hub' RESTful [API interface](https://dataspace.copernicus.eu/analyse/apis/sentinel-hub) to access various satellite imagery archives. It allows you to access raw satellite data, rendered images, statistical analysis, and other features.

## Installation

### Stable version

You can install the current stable version of `CDSE` from [CRAN](https://cran.r-project.org/package=CDSE) via:

``` r
install.packages("CDSE")
```

Windows and macOS binary packages are available from here.

### Development version

You can install the development version of `CDSE` with latest features from [GitHub](https://github.com/zivankaraman/CDSE) with:

``` r
# install.packages("remotes")
remotes::install_github("zivankaraman/CDSE")
```

## Usage

Access to the 'Copernicus Data Space Ecosystem' is free, but you have to register to get the authentication credentials required to use the API. These necessary steps are explained in detail in the [Before you start](https://zivankaraman.github.io/CDSE/articles/BeforeYouStart.html) article, so please read it first.

Once you have set up your working environment, the [Get started](https://zivankaraman.github.io/CDSE/articles/CDSE.html) will guide you through the main functions and demonstrate how to use the package.

## License

This package is released under [AGPL-3](https://cran.r-project.org/web/licenses/AGPL-3).
