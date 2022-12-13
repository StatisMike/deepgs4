#' @title Data Source
#' @description Information about an external data source in the spreadsheet.
#' @param dataSourceId The spreadsheet-scoped unique ID that identifies the data source.
#' @param spec object of [DataSourceSpec] class. Specifies data source connected
#' with this spreadsheet.
#' @param calculatedColumns object of class [DataSourceColumn] or list of such.
#' All calculated columns in the data source.
#' @param sheetId The ID of the Sheet connected with the data source. The field
#' cannot be changed once set.
#' @export

DataSource <- function(
    dataSourceId = NULL,
    spec = NULL,
    calculatedColumns = NULL,
    sheetId = NULL) {

  calculatedColumns <- nest_if_class(calculatedColumns, "DataSourceColumn") |>
    check_if_all_class("DataSourceColumn")

  obj <- list() |>
    append_cond(dataSourceId, type = "character") |>
    append_cond(spec, class = "DataSourceSpec") |>
    append_cond(calculatedColumns) |>
    append_cond(sheetId, type = "integer") |>
    dgs4_class("DataSource")

  return(obj)

}

#' @rdname DataSource
#' @param x any R object
#' @export
is.DataSource <- function(x) {
  is.dgs4_class(x, "DataSource")
}

#' @title DataSourceSpec
#' @description This specifies the details of the [DataSource]. For example,
#' for BigQuery, this specifies information about the BigQuery source.
#' @param bigQuery object of class [BigQueryDataSourceSpec]
#' @param parameters object of class [DataSourceParameter] or list of such.
#' The parameters of the data source, used when querying the data source.
#' @param ... other sources data
#' @export
DataSourceSpec <- function(
    bigQuery = NULL,
    parameters = NULL,
    ...) {

  parameters <- nest_if_class(parameters, "DataSourceParameter") |>
    check_if_all_class("DataSourceParameter")

  obj <- list() |>
    append_cond(bigQuery, class = "BigQueryDataSourceSpec") |>
    append_cond(parameters)

  dots <- list(...)
  obj <- c(obj, dots)

  obj <- dgs4_class(obj, "DataSourceSpec")

  return(obj)

}

#' @rdname DataSourceSpec
#' @param x any R object
#' @export
is.DataSourceSpec <- function(x) {
  is.dgs4_class(x, "DataSourceSpec")
}


#' @title BigQueryDataSourceSpec
#' @description The specification of a BigQuery data source that's connected to a sheet.
#' Exactly one of `querySpec` or `tableSpec` can be specified.
#' @param projectId The ID of a BigQuery enabled GCP project with a billing
#' account attached. For any queries executed against the data source, the project is charged.
#' @param querySpec object of class [BigQueryQuerySpec]
#' @param tableSpec object of class [BigQueryTableSpec]
#' @export
BigQueryDataSourceSpec <- function(projectId, querySpec = NULL, tableSpec = NULL) {

  spec_null <- vapply(list(querySpec, tableSpec), is.null, logical(1))

  if (sum(spec_null) != 1)
    dgs4_error("Exactly one of {.arg querySpec} or {.arg tableSpec} needs to be specified.")

  obj <- list() |>
    append_cond(projectId, type = "character", skip_null = FALSE) |>
    append_cond(querySpec, class = "BigQueryQuerySpec") |>
    append_cond(tableSpec, class = "BigQueryTableSpec") |>
    dgs4_class("BigQueryDataSourceSpec")

  return(obj)

}

#' @rdname BigQueryDataSourceSpec
#' @param x any R object
#' @export
is.BigQueryDataSourceSpec <- function(x) {
  is.dgs4_class(x, "BigQueryDataSourceSpec")
}

#' @title BigQueryTableSpec
#' @description Specifies a BigQuery table definition. Only native tables is allowed.
#' @param tableId The BigQuery table id.
#' @param datasetId The BigQuery dataset id.
#' @param tableProjectId The ID of a BigQuery project the table belongs to. If not specified, the projectId is assumed.
#' @export
BigQueryTableSpec <- function(tableId, datasetId, tableProjectId = NULL) {

  obj <- list() |>
    append_cond(tableId, type = "character") |>
    append_cond(datasetId, type = "character") |>
    append_cond(tableProjectId, type = "character") |>
    dgs4_class("BigQueryTableSpec")

  return(obj)

}

#' @rdname BigQueryTableSpec
#' @param x any R object
#' @export
is.BigQueryTableSpec <- function(x) {
  is.dgs4_class(x, "BigQueryTableSpec")
}

#' @title BigQueryQuerySpec
#' @description Specifies a custom BigQuery query.
#' @param rawQuery The raw query string.
#' @export
BigQueryQuerySpec <- function(rawQuery) {

  obj <- check_if_type(rawQuery, type = "character") |>
    dgs4_class("BigQuerySpec")

  return(obj)

}

#' @rdname BigQueryQuerySpec
#' @param x any R object
#' @export
is.BigQueryQuerySpec <- function(x) {
  is.dgs4_class(x, "BigQueryQuerySpec")
}

#' @title DataSourceParameter
#' @description A parameter in a data source's query. The parameter allows
#' the user to pass in values from the spreadsheet into a query. Exactly
#' one of `namedRangeId` or `range` needs to be specified.
#' @param name Named parameter. Must be a legitimate identifier for the [DataSource]
#' that supports it. For example, BigQuery identifier.
#' @param namedRangeId ID of a [NamedRange.] Its size must be 1x1.
#' @param range A [GridRange] that contains the value of the parameter. Its size must be 1x1.
#' @export
DataSourceParameter <- function(name, namedRangeId = NULL, range = NULL) {

  range_null <- vapply(list(querySpec, tableSpec), is.null, logical(1))

  if (sum(range_null) != 1)
    dgs4_error("Exactly one of {.arg namedRangeId} or {.arg range} needs to be specified.")

  obj <- list() |>
    append_cond(name, type = "character", skip_null = FALSE) |>
    append_cond(namedRangeId, type = "character") |>
    append_cond(range, class = "GridRange") |>
    dgs4_class("DataSourceParameter")

  return(obj)

}

#' @rdname DataSourceParameter
#' @param x any R object
#' @export
is.DataSourceParameter <- function(x) {
  is.dgs4_class(x, "DataSourceParameter")
}

#' @title DataSourceRefreshSchedule
#' @description
#' Schedule for refreshing the [DataSource].
#'
#' At most one of `daysOfWeek` or `daysOfMonth` can be specified.
#'
#' @param enabled `TRUE` if the refresh schedule is enabled, or false otherwise.
#' @param startTime object of class [TimeOfDay] specifying the startTime of schedule
#' refresh. If neither `daysOfWeek` or `daysOfMonth` are specified, refresh will
#' be made daily
#' @param daysOfWeek day of the week when the refresh should be made.
#' @param daysOfMonth days of month when the refresh should be made. Integers
#' between 1 and 28 inclusive are valid.
#' @param nextRun **READ ONLY** The time interval of the next run. Object of
#' class [Interval]
#' @param ... for support of some redundant SheetsAPI fields
#' @details
#'
#' ## daysOfWeek options:
#' MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
#'
#' ## Exact refresh time
#' Data sources in the spreadsheet are refreshed within a time interval. You
#' can specify the start time by clicking the Scheduled Refresh button in the
#' Sheets editor, but the interval is fixed at 4 hours. For example, if you
#' specify a start time of 8am , the refresh will take place between 8am and
#' 12pm every day.
#' @export

DataSourceRefreshSchedule <- function(
    enabled = NULL,
    startTime = NULL,
    daysOfWeek = NULL,
    daysOfMonth = NULL,
    nextRun = NULL,
    ...) {

  days_null <- vapply(list(daysOfWeek, daysOfMonth), is.null, logical(1))

  if (sum(days_null) == 0)
    dgs4_error("At most one of {.arg daysOfWeek} or {.arg daysOfMonth} can be specified.")

  daysOfWeek <- unique(daysOfWeek) |>
    check_if_options(
    "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY",
    "SATURDAY", "SUNDAY",
    max_length = 7
  )

  if (!is.null(daysOfMonth)) {
    if (!is.numeric(daysOfMonth))
      dgs4_error("{.arg daysOfMonth} needs to be a {.class numeric} vector.")
    if (any(daysOfMonth > 28) || any(daysOfMonth < 1))
      dgs4_error("Only values between {.val 1} and {.val 28} (inclusive) are valid for {.arg daysOfMonth}")
  }

  obj <- list() |>
    append_cond(enabled, type = "logical") |>
    append_cond(startTime, class = "TimeOfDay") |>
    append_cond(daysOfWeek) |>
    append_cond(daysOfMonth) |>
    append_cond(nextRun, class = "Interval") |>
    dgs4_class("DataSourceRefreshSchedule")

  return(obj)

}

#' @rdname DataSourceRefreshSchedule
#' @param x any R object
#' @export
is.DataSourceRefreshSchedule <- function(x) {
  is.dgs4_class(x, "DataSourceRefreshSchedule")
}

#' @title DataSourceColumn
#' @description A column in a [DataSource].
#' @param reference The display name of the column. It should be unique within a data source.
#' @param formula The formula of the calculated column.
#' @export
DataSourceColumn <- function(reference,
                             formula = NULL) {

  obj <- list() |>
    append_cond(reference, type = "character") |>
    append_cond(formula, type = "character") |>
    dgs4_class("DataSourceColumn")

  return(obj)

}

#' @rdname DataSourceColumn
#' @param x any R object
#' @export
is.DataSourceColumn <- function(x) {
  is.dgs4_class(x, "DataSourceColumn")
}

#' @title TimeOfDay
#' @description
#' Represents a time of day. The date and time zone are either not significant
#' or are specified elsewhere. An API may choose to allow leap seconds
#' @param hours Hours of day in 24 hour format. Should be from 0 to 23. An API
#' may choose to allow the value `24` for scenarios like business closing time.
#' @param minutes Minutes of hour of day. Must be from 0 to 59.
#' @param seconds Seconds of minutes of the time. Must normally be from 0 to 59.
#' An API may allow the value 60 if it allows leap-seconds.
#' @param nanos Fractions of seconds in nanoseconds. Must be from 0 to 999,999,999.
#' @export
TimeOfDay <- function(hours = NULL,
                      minutes = NULL,
                      seconds = NULL,
                      nanos = NULL) {

  out <- list() |>
    append_cond(hours, type = "integer") |>
    append_cond(minutes, type = "integer") |>
    append_cond(seconds, type = "integer") |>
    append_cond(nanos, type = "integer") |>
    dgs4_class("TimeOfDay")

  if (!is.null(hours) && (hours < 0 || hours > 24))
    dgs4_error("{.arg hours} need to be between {.val 0} and {.val 23}. {.val 24} is also enabled for some scenarios")

  if (!is.null(minutes) && (minutes < 0 || minutes > 59))
    dgs4_error("{.arg minutes} need to be between {.val 0} and {.val 59}.")

  if (!is.null(seconds) && (seconds < 0 || seconds > 60))
    dgs4_error("{.arg seconds} need to be between {.val 0} and {.val 59}. {.val 60} is also enabled for some scenarios")

  if (!is.null(nanos) && (nanos < 0 || nanos > 999999999))
    dgs4_error("{.arg nanos} need to be between {.val 0} and {.val 999999999}.")

  return(out)

}

#' @rdname TimeOfDay
#' @param x any R object
#' @export
is.TimeOfDay <- function(x) {
  is.dgs4_class(x, "TimeOfDay")
}

#' @title Interval
#' @description Represents a time interval, encoded as a Timestamp start (inclusive)
#' and a Timestamp end (exclusive).
#'
#' The start must be less than or equal to the end. When the start equals the
#' end, the interval is empty (matches no time). When both start and end are
#' unspecified, the interval matches any time.
#' @param startTime Optional. Inclusive start of the interval. If specified, a
#' Timestamp matching this interval will have to be the same or after the start.
#' @param endTime Optional. Exclusive end of the interval.
#' If specified, a Timestamp matching this interval will have to be before the end.
#' @export
Interval <- function(startTime = NULL,
                     endTime = NULL) {

  obj <- list() |>
    append_cond(startTime, type = "character") |>
    append_cond(endTime, type = "character") |>
    dgs4_class("Interval")

  return(obj)

}

#' @rdname Interval
#' @param x any R object
#' @export
is.Interval <- function(x) {
  is.dgs4_class(x, "Interval")
}

#' @title DataExecutionStatus
#' @description Object returned from GoogleSheet API with details of current
#' execution of data retrieval for **DATA_SOURCE** Sheet
#' @param state The state of the data execution.
#' @param errorCode The error code.
#' @param errorMessage errorMessage
#' @param lastRefreshTime Gets the time the data last successfully refreshed.
#' @details
#' ## Possible states:
#' - **NOT_STARTED**: The data execution has not started.
#' - **RUNNING**: The data execution has started and is running.
#' - **SUCCEEDED**: The data execution has completed successfully.
#' - **FAILED**: The data execution has completed with errors.
#'
#' ## Possible `errorCode`s:
#' - **TIMED_OUT**: The data execution timed out.
#' - **TOO_MANY_ROWS**: The data execution returns more rows than the limit.
#' - **TOO_MANY_COLUMNS**: The data execution returns more columns than the limit.
#' - **TOO_MANY_CELLS**: The data execution returns more cells than the limit.
#' - **ENGINE**: Error is received from the backend data execution engine
#' (e.g. BigQuery). Check errorMessage for details.
#' - **PARAMETER_INVALID**: One or some of the provided data source parameters
#' are invalid.
#' - **UNSUPPORTED_DATA_TYPE**: The data execution returns an unsupported data type.
#' - **DUPLICATE_COLUMN_NAMES**: The data execution returns duplicate column
#' names or aliases.
#' - **INTERRUPTED**: The data execution is interrupted. Please refresh later.
#' - **CONCURRENT_QUERY**: The data execution is currently in progress, can
#' not be refreshed until it completes.
#' - **OTHER**: Other errors.
#' - **TOO_MANY_CHARS_PER_CELL**: The data execution returns values that exceed
#' the maximum characters allowed in a single cell.
#' - **DATA_NOT_FOUND**:	The database referenced by the data source is not found.
#' - **PERMISSION_DENIED**: The user does not have access to the database
#' referenced by the data source.
#' - **MISSING_COLUMN_ALIAS**: The data execution returns columns with missing
#' aliases.
#' - **OBJECT_NOT_FOUND**: The data source object does not exist.
#' - **OBJECT_IN_ERROR_STATE**: The data source object is currently in error
#' state. To force refresh, set force in RefreshDataSourceRequest .
#' - **OBJECT_SPEC_INVALID**: The data source object specification is invalid.
#' @export

DataExecutionStatus <- function(
    state = c("NOT_STARTED", 	"RUNNING", "SUCCEEDED", "FAILED"),
    errorCode = NULL,
    errorMessage = NULL,
    lastRefreshTime = NULL) {

  state <- rlang::arg_match(state)

  errorCode <- check_if_options(
    errorCode, "TIMED_OUT", "TOO_MANY_ROWS", "TOO_MANY_COLUMNS", "TOO_MANY_CELLS",
    "ENGINE", "PARAMETER_INVALID","UNSUPPORTED_DATA_TYPE", "DUPLICATE_COLUMN_NAMES",
    "INTERRUPTED", "CONCURRENT_QUERY", "OTHER", "TOO_MANY_CHARS_PER_CELL",
    "DATA_NOT_FOUND", "PERMISSION_DENIED", "MISSING_COLUMN_ALIAS", "OBJECT_NOT_FOUND",
    "OBJECT_IN_ERROR_STATE", "OBJECT_SPEC_INVALID")

  obj <- list() |>
    append_cond(state) |>
    append_cond(errorCode) |>
    append_cond(errorMessage, type = "character")  |>
    append_cond(lastRefreshTime, type = "character") |>
    dgs4_class("DataExecutionStatus")

  return(obj)

}

#' @rdname DataExecutionStatus
#' @param x any R object
#' @export
is.DataExecutionStatus <- function(x) {
  is.dgs4_class(x, "DataExecutionStatus")
}
