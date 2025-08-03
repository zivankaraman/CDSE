#' Create a Sentinel Hub API Evalscript
#'
#' @description
#' Generates a JavaScript evalscript for calculating a spectral index for
#' Sentinel-1 or Sentinel-2 imagery.
#'
#' @details
#' This function takes a spectral index definition and creates a JavaScript
#' evalscript compatible with the Sentinel Hub API. The index can be specified
#' by its short name from the \code{rsi} package's spectral index database
#' (see \code{rsi::spectral_indices()}) or as a custom \code{list} or \code{data.frame}.
#'
#' The function automatically maps common band names (e.g., `N`, `R`, `G`, `B`)
#' to the corresponding Sentinel-1 or Sentinel-2 band names required by the
#' API. Any constants in the index formula (e.g., the soil adjustment factor
#' `L` in SAVI) must be provided as named arguments.
#'
#' @param x A \code{character} string with the \code{short_name} of a spectral index
#'   from \code{rsi::spectral_indices()} or a single-row \code{data.frame} (or \code{list})
#'   with the elements \code{bands}, \code{formula}, \code{platforms},
#'   and optionally \code{long_name}.
#' @param ... Named arguments providing values for any constants required by the
#'   index's formula. The function will stop if a required constant is missing
#'   and warn if unused arguments are provided.
#'
#' @return A `character` vector containing the JavaScript evalscript. It can be used as
#' `script` argument in functions that require one as shown here:
#'
#' \code{..., script = paste(MakeEvalScript("NDVI"), collapse = "\n"), ...}.
#'
#' It can also be saved to a file for later use or further modifications.
#'
#' @export
#'
#' @importFrom stats as.formula setNames
#'
#' @examples
#' \dontrun{
#'
#' # NDVI
#' si <- rsi::spectral_indices() # retrieves spectral indices
#' ndvi <- subset(si, short_name == "NDVI") # creates one-row data.frame
#' ndvi_script <- MakeEvalScript(ndvi) # generates the script
#' cat(ndvi_script, sep = "\n")
#'
#' # SAVI, which requires the constant L
#' cat(MakeEvalScript("SAVI", L = 0.5), sep = "\n", file = "SAVI.js")
#'
#' # Using a custom index definition as a data.frame
#' custom_index <- data.frame(
#'   short_name = "GNDVI",
#'   long_name = "Green Normalized Difference Vegetation Index",
#'   platforms = I(list("Sentinel-2")),
#'   bands = I(list(c("N", "G"))),
#'   formula = "(N - G) / (N + G)"
#' )
#' cat(MakeEvalScript(custom_index))
#' }
MakeEvalScript <- function(x, ...) {
    if (is.character(x)) {
        stopifnot(length(x) == 1L)
        # check that rsi package is available
        if (system.file(package = "rsi") == "") {
            stop("If argument 'x' is character the package 'rsi' is required", call. = FALSE)
        }
        si <- rsi::spectral_indices()
        x <- subset(si, si$short_name == x)
    }
    if (inherits(x, "data.frame")) stopifnot(nrow(x) == 1L)
    stopifnot(is.list(x))
    stopifnot(length(setdiff(c("bands", "formula", "platforms"), names(x))) == 0L)
    is_s1 <- !is.na(pmatch("Sentinel-1", unlist(x$platforms)))
    is_s2 <- !is.na(match("Sentinel-2", unlist(x$platforms)))
    stopifnot(isFALSE(all(c(is_s1, is_s2))))
    if (!(any(is_s1, is_s2))) {
        stop("This function is available only for platforms 'Sentinel-1' or 'Sentinel-2'")
    }
    bands <- unlist(x$bands)
    formula <- x$formula
    vars <- get_si_vars(formula)
    stopifnot(length(setdiff(vars, bands)) == 0L)
    if (is_s2) {
        map <-
            c(A = "B01", B = "B02", G = "B03", R = "B04", RE1 = "B05", RE2 = "B06",
              RE3 = "B07", N = "B08", N2 = "B8A", WV = "B09", S1 = "B11", S2 = "B12"
            )
    } else {
        map <- c(VV = "VV", VH = "VH",  HH = "HH", HV = "HV")
    }
    const <- setdiff(bands, names(map))
    bands <- intersect(bands, names(map))
    if (length(bands) == 0L) {
        stop("This function can't handle inidces that don't use any raw spectral bands")
    }
    dots <- list(...)
    missing <- setdiff(const, names(dots))
    if (length(missing) > 0L) {
        stop(sprintf("missing argument(s): %s", paste(missing, collapse = ", ")))
    }
    extra <- setdiff(names(dots), const)
    if (length(extra) > 0L) {
        warning(sprintf("unused argument(s): %s", paste(extra, collapse = ", ")))
    }
    bands <- sort(as.character(map[bands]))
    # prepend "sample." and keep the map names
    map[] <- paste0("sample.", map)
    # update formula with band names used by API
    formula <- rename_si_vars(formula, map)
    # create the evalscript
    make_script(bands, dots[const], formula, x$long_name)
}

make_script <- function(bands, const, formula, long_name) {
    header <- "//VERSION=3"
    suppressWarnings(
        if (!is.null(long_name)) {
            header <- c(header, sprintf("//%s", long_name))
        }
    )
    # define 'setup' function
    setup <- c(
        'function setup() {',
        '  return {',
        '    input: [ // this sets which bands to use',
        '      {',
        '        bands: [',
        sprintf('          %s,', paste0('"', bands, '"')),
        '          "dataMask"',
        '        ],',
        '      },',
        '   ],',
        '  output: [ // this defines the output image type',
        '    {',
        '      id: "default",',
        '      bands: ["index"],',
        '      sampleType: "FLOAT32"',
        '    },',
        '    {',
        '      id: "dataMask",',
        '      bands: 1',
        '    }]',
        '  }',
        '}')
    # define custom parameters, if any
    if (length(const) > 0) {
        const <- c(
            "  // initialize parameters",
            sapply(seq_along(const), FUN = function(i, x) {
                sprintf("  const %s = %s;", names(x)[i], x[[i]])
            }, const))
    } else {
        const <- character(0)
    }
    # define evaluatePixel function
    evalPix <- c(
        "function evaluatePixel(sample) {",
        const,
        "  // this computes the index value",
        sprintf("  let ndx = %s;", formula),
        "  return {",
        "    default: [ndx],",
        "    dataMask: [sample.dataMask]",
        "  }",
        "}")

    script <- c(header, "", setup, "", evalPix)
    # result
    return(script)
}

get_si_vars <- function(x) {
    # Convert to a one-sided formula
    formula_obj <- as.formula(paste("~", x))
    # Extract variable names
    vars <- all.vars(formula_obj)
}

rename_si_vars <- function(expr_string, rename_map) {
    # Convert to formula
    formula_obj <- as.formula(paste("~", expr_string))
    # Create substitution list: original symbol names to new symbol names
    subs <- setNames(lapply(rename_map, as.name), names(rename_map))
    # Extract the RHS of the formula
    rhs_expr <- formula_obj[[2]]
    # Perform substitution
    rhs_expr_renamed <- do.call(substitute, list(rhs_expr, subs))
    # Convert back to character
    new_expr_string <- paste(deparse(rhs_expr_renamed), collapse = "")
    # Remove multiple spaces
    new_expr_string <- gsub("\\s+", " ", new_expr_string)
    # Result
    return(new_expr_string)
}
