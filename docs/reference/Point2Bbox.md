# Create bounding box around a point

Creates the bounding box (numeric vector of length four) around the
input point(s).

## Usage

``` r
Point2Bbox(x, y = NULL, size, crs = 4326)
```

## Arguments

- x:

  an `sf`, `sfc`, or `SpatialPoints*` object, a numeric indicating the
  longitude/easting of the point(s), or any input accepted by
  `xy.coords`

- y:

  numeric, the latitude/northing of the point(s). Default: NULL

- size:

  numeric indicating the size (in meters) of the bounding box to create

- crs:

  coordinate reference system of the input (and the output): object of
  class `crs`, or input string for `st_crs`. Default: 4326 (WGS 84)

## Value

A bounding box (numeric vector of length four), or a list of bounding
boxes if the input is not scalar.

## Details

The function assumes that the `crs` units are either degrees or meters,
a warning is issued if not, and the result will probably be incorrect.

## See also

[`xy.coords`](https://rdrr.io/r/grDevices/xy.coords.html),
[`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html)

## Examples

``` r
if (FALSE) { # \dontrun{
Point2Bbox(x = -73.96557, y = 40.78246, size = 1000, crs = 4326)
} # }
```
