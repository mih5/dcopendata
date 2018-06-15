#' Access datasets related to crime incidents in the District of Columbia
#'
#' @param time_period The time_period of interest (from \code{"2008"} to \code{"2018"}) as a string, or \code{"Last 30 Days"}.
#' @param where_condition A string giving a
#' @param record_offset An offset used to skip records before gathering them, default 0
#' @param record_count Number of records to return, default 1000
#'
#' @importFrom magrittr "%>%"
#'
#' @details
#'
#'

dc_crime_incidents <- function(
  time_period = c(2008:2018, "Last 30 Days"),
  where_condition = NULL,
  sort_condition = NULL,
  record_offset = 0,
  record_count = 1000
  ){

  # parameter checking
  time_period <- match.arg(time_period, choices=c(as.character(2008:2018),  "Last 30 Days"))

  base_path <- "/dcgis/rest/services/FEEDS/MPD/MapServer"
  # Get the ids for the tables for the MPD's crime incidents by querying the base path (which returns a JSON with an element describing the layers)
  id_resp <- dcopendata_api(
    path = paste0(base_path, "/query"),
    query = list(f="json")
  )
  MPD_tables <- purrr::map_dfr(id_resp$content$layers, .f=function(x){
      dplyr::as_data_frame(Filter(Negate(is.null), x)) # filter out elements of x which are null, then convert to tibble
    }) %>%
    dplyr::mutate(
      time_period = ifelse(stringr::str_detect(name, "Last 30 Days"), "Last 30 Days", stringr::str_extract(name, "20[0-9]{2}"))
    )

  # Get the data from the relevant year, and with the relevant query parameters
  table_id <- MPD_tables$id[which(MPD_tables$time_period==time_period)]
  query <- list(
    where = where_condition,
    orderByFields = sort_condition,
    outFields = "*",
    outSR = 4326,
    f = "json"
  )

  # query the api to get the record count (not returned)
  record_count <- dcopendata_api(
    path = paste0(base_path, "/", table_id, "/query"),
    query = c(query, returnCountOnly=TRUE)
  )
  if(record_count$content$count > resultRecordCount)
  # query the api to get the data
  data_resp <- dcopendata_api(
    path = paste0(base_path, "/", table_id, "/query"),
    query = c(query, resultOffset=record_offset, resultRecordCount=record_count)
    )

  # munge the data
  purrr::map_dfr(data_resp$content$features, .f=function(x){
    dplyr::as_data_frame(Filter(Negate(is.null), x$attributes)) # filter out elements of x$attributes which are null, then convert to tibble
  })

}

