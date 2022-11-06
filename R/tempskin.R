#Constants
url_skin <- "https://api.fitbit.com/1/user/-/temp/skin/"

#' @title Get skin temp
#'
#' @description
#'  \code{get_heart_rate_time_series()} returns time series data in the specified range
#'   If you specify earlier dates in the request, the response will retrieve only data since the user's join date or the first log entry date for the requested collection.
#'
#' @inheritParams inheritparams_token
#' @param date The end date of the period specified in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param period The range for which data will be returned. Options are "1d", "7d", "30d", "1w", "1m".
#' @param base_date The range start date in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param end_date The end date of the range in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @inheritParams inheritparams_simplify
#'
#' @details
#' See \url{https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-time-series} for more details.
#'
#' @export
get_temp_skin <- function(token, date, simplify=TRUE)
{
  url <- paste0(url_skin, sprintf("date/%s.json", format_date(date)))
  # We can not simplify this output because it is so complicated nested list
  tidy_output(get(url, token), simplify=FALSE)
}

#' @title Get Heart Rate Intraday Time Series
#'
#' @description
#'   \code{get_heart_rate_intraday_time_series()} returns the intraday time series.
#'   If your application has the appropriate access, your calls to a time series endpoint for a specific day (by using start and end dates on the same day or a period of 1d),
#'   the response will include extended intraday values with a one-minute detail level for that day.
#'   Access to the Intraday Time Series for personal use (accessing your own data) is available through the "Personal" App Type.
#'
#' @inheritParams inheritparams_token
#' @param date The end date of the period specified in the format "yyyy-MM-dd" or "today" as a string or Date object.
#' @param detail_level Number of data points to include. Either "1sec" or "1min".
#' @param start_time The start of the period, in the format "HH:mm".
#' @param end_time The end of the period, in the format "HH:mm"
#' @inheritParams inheritparams_simplify
#'
#' @details
#' See \url{'https://dev.fitbit.com/reference/web-api/heart-rate/#get-heart-rate-intraday-time-series} for more details.
#
get_temp_skin <- function(token, resource_path, date="", period="", base_date="", end_date="", simplify=TRUE)
{
  url <- if(date != "" && period != ""){
    paste0(url_skin, sprintf("%s/date/%s/%s.json", resource_path, format_date(date), period))
  } else if(base_date != "" & end_date != ""){
    paste0(url_skin, sprintf("%s/date/%s/%s.json", resource_path, format_date(base_date), format_date(end_date)))
  } else{
    stop("Error: Need to enter combination of date/period or base_date/end_date")
  }
  tidy_output(get(url, token), simplify)
}