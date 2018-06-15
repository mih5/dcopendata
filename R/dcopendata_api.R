#' Access the District of Columbia Open Data API
#'
#' @param path Path component of URL
#' @param query Query component of URL
#'
#' @return A list with class \code{dcopendata_api} containing \code{url} (the url of the request),
#' \code{content} (the parsed JSON content of the request as a list), and \code{response} (the original response).
#'
#' @author Mao Hu
#'
#' @details
#' This function accesses the District of Columbia's Open Data API. More information about DC's open data portal
#' and interesting visualizations they have can be found at \url{http://opendata.dc.gov}.
#'
#' For more information about the API, please see \url{http://opendata.dc.gov/pages/using-apis}. The API documentation
#' can be found at \url{https://developers.arcgis.com/rest/services-reference/query-feature-service-layer-.htm#GUID-EE6C21C1-03E8-4379-9EF0-44F38AA98779}.
#'
#' @export

dcopendata_api <- function(
  path,
  query = NULL
  ){

  ua <- httr::user_agent("https://github.com/mih5/dcopendata")
  base_url <- "https://maps2.dcgis.dc.gov"
  url <- httr::modify_url(url=base_url, path=path, query=query)

  #url <- curl::curl_unescape(url) # need to unescape because ARCGIS uses non-ascii characters in API
  response <- httr::GET(url)

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)

  # error handling
   if (httr::http_error(response)) {
     stop(
       sprintf(
         "DC Open Data API request failed [%s]",
         httr::status_code(response)
       ),
       call. = FALSE
     )
   }

  # return structure
  structure(
    list(
      url = url,
      content = parsed,
      response = response
    ),
    class = "dcopendata_api"
  )

}

# print method
print.dcopendata_api <- function(x, ...){
  cat("<DC Open Data>\n", sep = "")
  str(x$content)
  invisible(x)
}
