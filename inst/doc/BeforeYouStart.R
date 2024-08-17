## ----label = "setup", include = FALSE-----------------------------------------
library(CDSE)

## ----label = "GetOAuthClient", eval = FALSE-----------------------------------
#  id <- Sys.getenv("CDSE_ID")
#  secret <- Sys.getenv("CDSE_SECRET")
#  OAuthClient <- GetOAuthClient(id = id, secret = secret)
#  class(OAuthClient)
#  #> [1] "httr2_oauth_client"
#  OAuthClient
#  #> <httr2_oauth_client>
#  #> name: x9x99xx99x9xx99xx99xx9xx99x99x99
#  #> id: xx-9x999x9x-9999-999x-xxxx-x9999x99x99x
#  #> secret: <REDACTED>
#  #> token_url: https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token
#  #> auth: oauth_client_req_auth_header

## ----label = "Dummy OAuthClient", eval = TRUE---------------------------------
id <- "my_dummy_id"
secret <- "my_dummy_secret"
OAuthClient <- GetOAuthClient(id = id, secret = secret, url = "https://my_dummy_url.org")
class(OAuthClient)
OAuthClient

## ----label = "GetOAuthToken", eval = FALSE------------------------------------
#  id <- Sys.getenv("CDSE_ID")
#  secret <- Sys.getenv("CDSE_SECRET")
#  OAuthToken <- GetOAuthToken(id = id, secret = secret)
#  class(OAuthToken)
#  #> [1] "character"
#  OAuthToken
#  #> [1] "xxXxxXxxXxXXXxX9XxXxXxX9xXXxXxXxxXXxxxx9xxXxX9XXXXx9X9xXxX9XxXXXxxX9xXXXxx......"

