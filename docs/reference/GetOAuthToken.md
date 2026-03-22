# Get OAuth token

Gets an OAuth authentication token (long character string)

## Usage

``` r
GetOAuthToken(id, secret, url = getOption("CDSE.auth_url"))
```

## Source

<https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Overview/Authentication.html>

## Arguments

- id:

  character, user OAuth client id

- secret:

  character, user OAuth client secret

- url:

  character, endpoint for requesting tokens. Default: Copernicus Data
  Space Ecosystem OAuth endpoint

## Value

Long character string containing the authentication token.

## Details

The token can be used in queries requiring the authentication.

## See also

[`GetOAuthClient`](https://zivankaraman.github.io/CDSE/reference/GetOAuthClient.md)

## Examples

``` r
if (FALSE) { # \dontrun{
id <- "..."
secret <- "..."
token <- GetOAuthToken(id = id, secret = secret)
} # }
```
