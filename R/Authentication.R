#' @title Get an OAuth token
#' @description Gets an OAuth authentication token (long character string)
#' @param id character, user OAuth client id
#' @param secret character, user OAuth client secret
#' @param url character, endpoint for requesting tokens. Default: Copernicus Data Space Ecosystem OAuth endpoint
#' @return Long character string containing the authentication token.
#' @details The token can be used in queries requiring the authentication.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[CDSE]{GetClient}}
#' @rdname GetToken
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Overview/Authentication.html}
#' @importFrom httr2 oauth_client oauth_flow_client_credentials
GetToken <- function(id, secret, url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token") {
    client <- httr2::oauth_client(id = id, token_url = url,secret = secret, auth = "header")
    token <- httr2::oauth_flow_client_credentials(client)
    out <- token$access_token
    attr(out, "expires") <- as.POSIXct(token$expires_at, origin = "1970-01-01")
    return(out)
}

#' @title Get an OAuth client
#' @description Gets an OAuth authentication client (\code{httr2} OAuth client object)
#' @param id character, user OAuth client id
#' @param secret character, user OAuth client secret
#' @param url character, endpoint for requesting tokens. Default: Copernicus Data Space Ecosystem OAuth endpoint
#' @return \code{httr2} OAuth client object
#' @details The client can be used in queries requiring the authentication.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[CDSE]{GetToken}}
#' @rdname GetClient
#' @export
#' @source \url{https://documentation.dataspace.copernicus.eu/APIs/SentinelHub/Overview/Authentication.html}
#' @importFrom httr2 oauth_client
GetClient <- function(id, secret, url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token") {
    client <- httr2::oauth_client(id = id, token_url = url, secret = secret, auth = "header")
    return(client)
}
