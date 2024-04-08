.onLoad <- function(libname, pkgname) {
    options(
    CDSE.auth_url = "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token",
    CDSE.catalog_url = "https://sh.dataspace.copernicus.eu/api/v1/catalog/1.0.0/",
    CDSE.process_url = "https://sh.dataspace.copernicus.eu/api/v1/process",
    CDSE.statistical_url = "https://sh.dataspace.copernicus.eu/api/v1/statistics")
}
