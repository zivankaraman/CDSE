# Create a Sentinel Hub API Evalscript

Generates a JavaScript evaluation script for calculating a spectral
index for Sentinel-1, Sentinel-2 or Landsat-8/9 imagery.

## Usage

``` r
MakeEvalScript(
  x,
  ...,
  constellation = c("sentinel-1", "sentinel-2", "landsat")
)
```

## Arguments

- x:

  A `character` string with the `short_name` of a spectral index from
  [`rsi::spectral_indices()`](https://permian-global-research.github.io/rsi/reference/spectral_indices.html)
  or a single-row `data.frame` (or `list`) with the elements `bands`,
  `formula`, `platforms`, and optionally `long_name`.

- ...:

  Named arguments providing values for any constants required by the
  index's formula. The function will stop if a required constant is
  missing and warn if unused arguments are provided.

- constellation:

  character indicating the constellation for which the script should be
  generated. Must be one of "sentinel-1", "sentinel-2", or "landsat".

## Value

A \`character\` vector containing the JavaScript evalscript. It can be
used as \`script\` argument in functions that require one as shown here:

`..., script = paste(MakeEvalScript("NDVI"), collapse = "\n"), ...`.

It can also be saved to a file for later use or further modifications.

## Details

This function takes a spectral index definition and creates a JavaScript
evaluation script compatible with the Sentinel Hub API. The index can be
specified by its short name from the `rsi` package's spectral index
database (see
[`rsi::spectral_indices()`](https://permian-global-research.github.io/rsi/reference/spectral_indices.html))
or as a custom `list` or `data.frame`.

The function automatically maps common band names (e.g., \`N\`, \`R\`,
\`G\`, \`B\`) to the corresponding Sentinel-1, Sentinel-2 or Landsat-8/9
band names required by the API, based on the value of the
`constellation` argument. Any constants in the index formula (e.g., the
soil adjustment factor \`L\` in SAVI) must be provided as named
arguments.

## Examples

``` r
if (FALSE) { # \dontrun{

# NDVI
si <- rsi::spectral_indices() # retrieves spectral indices
ndvi <- subset(si, short_name == "NDVI") # creates one-row data.frame
ndvi_script <- MakeEvalScript(ndvi, constellation = "sentinel-2") # generates the script
cat(ndvi_script, sep = "\n")

# SAVI, which requires the constant L
cat(MakeEvalScript("SAVI", L = 0.5), sep = "\n", file = "SAVI.js")

# Using a custom index definition as a data.frame
custom_index <- data.frame(
  short_name = "GNDVI",
  long_name = "Green Normalized Difference Vegetation Index",
  platforms = I(list("Sentinel-2")),
  bands = I(list(c("N", "G"))),
  formula = "(N - G) / (N + G)"
)
cat(MakeEvalScript(custom_index, constellation = "landsat"))
} # }
```
