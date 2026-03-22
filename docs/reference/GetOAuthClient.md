# Get OAuth client

Gets an OAuth authentication client (`httr2` OAuth client object)

## Usage

``` r
GetOAuthClient(id, secret, url = getOption("CDSE.auth_url"))
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

`httr2` OAuth client object

## Details

The client can be used in queries requiring the authentication.

## See also

[`GetOAuthToken`](https://zivankaraman.github.io/CDSE/reference/GetOAuthToken.md)

## Examples

``` r
if (FALSE) { # \dontrun{
id <- "..."
secret <- "..."
OAuthClient <- GetOAuthClient(id = id, secret = secret)
} # }
```
