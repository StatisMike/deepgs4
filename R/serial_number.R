#' @title Coerce Dates to googlesheets Serial Number
#' @param date value to convert. It needs to be a value recognized by
#' [lubridate::as_datetime()]
#' @details
#' "Serial number" format, as popularized by Lotus 1-2-3. The whole number
#' portion of the value (left of the decimal) counts the days since December
#' 30th 1899. The fractional portion (right of the decimal) counts the time
#' as a fraction of the day. For example, January 1st 1900 at noon would be
#' `2.5`, `2` because it's 2 days after December 30th 1899, and `.5` because
#' noon is half a day. February 1st 1900 at 3pm would be `33.625`.
#' @export
dgs4_serial_number <- function(date) {

  diff <- lubridate::as_datetime(date) - lubridate::as_datetime("1899-12-30")
  diff_days <- as.numeric(diff, unit = "days")
  class(diff_days) <- "dgs4_serial_number"

  return(diff_days)

}

#' @rdname dgs4_serial_number
#' @param x object of class `dgs4_serial_number` to coerce
#' @inheritParams base::strptime
#' @export
as.character.dgs4_serial_number <- function(
    x,
    format = "",
    ...) {

  whole_days <- lubridate::as_datetime("1899-12-30") + lubridate::days(floor(x))
  fraction_day <- as.numeric(x - floor(x)) * 86400
  datetime <- whole_days + lubridate::seconds(fraction_day)

  format(datetime, format = format)

}

#' @rdname dgs4_serial_number
#' @export
as.POSIXct.dgs4_serial_number <- function(
    x,
    tz = "",
    ...) {

  as.character(x) |>
    as.POSIXct(tz = tz, ...)

}

#' @rdname dgs4_serial_number
#' @export
as.POSIXlt.dgs4_serial_number <- function(
    x,
    tz = "",
    ...) {

  as.character(x) |>
    as.POSIXlt(tz = tz, ...)

}

#' @rdname dgs4_serial_number
#' @export
as.Date.dgs4_serial_number <- function(
    x,
    ...) {

  as.character(x) |>
    as.Date(...)

}

#' @rdname dgs4_serial_number
#' @export
is.dgs4_serial_number <- function(x) {
  inherits(x, "dgs4_serial_number")
}
