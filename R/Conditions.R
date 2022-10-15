#' @title Boolean Condition
#' @description Object representing condition that can evaluate to true or false.
#' These conditions are useb by conditional formatting, data validation and the
#' criteria in filters
#' @param type Condition type. Description of each condition type meaning can be
#' found in *details*
#' @param values object of class [ConditionValue] or list of such objects. Number
#' of supported values depends on type of the condition: some supporting zero,
#' other one or two an `"ONE_OF_LIST"` - arbitrary number. Information can be
#' found in *details*
#' @details
#' # Numeric-specific conditions:
#' - **NUMBER_GREATER**, **NUMBER_GREATER_THAN_EQ**
#'   - cell's value must be greater (or equal) than condition's value.
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue`
#' - **NUMBER_LESS**, **NUMBER_LESS_THAN_EQ**
#'   - cell's value must be less (or equal) than condition's value.
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue`
#' - **NUMBER_EQ**, **NUMBER_NOT_EQ**
#'   - cell's value must be equal (or not equal) to the condition's value
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue`. 1 or more for filters on data source objects.
#' - **NUMBER_BETWEEN**, **NUMBER_NOT_BETWEEN**
#'   - cell's value must be (or not be) between two condition values.
#'   - supported in: data validation, conditional formatting and filters
#'   - 2 `ConditionValue`s
#'
#' # Text-specific conditions:
#' - **TEXT_CONTAINS**, **TEXT_NOT_CONTAINS**
#'   - cells' value must contain/not contain the condition's value.
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue`
#' - **TEXT_STARTS_WITH**, **TEXT_ENDS_WITH**
#'   - cell's value must start/end with the condition's value.
#'   - supported in: conditional formatiing and filters
#'   - 1 `ConditionValue`
#' - **TEXT_EQ**
#'   - cell's value must be exactly the condition's value
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue`. 1 or more for filters on data source objects.
#' - **TEXT_IS_EMAIL**
#'   - cell's value must be a valid email address
#'   - supported in data validation
#'   - NO `ConditionValue`
#' - **TEXT_IS_URL**
#'   - cell's value must be a valid URL
#'   - supported in data validation
#'   - NO `ConditionValue`
#' - **TEXT_NOT_EQ**
#'   - cell's value must be exactly not the condition's value.
#'   - supported by filters on data source objects
#'   - 1 or more `ConditionValue`s
#'
#' # Dates-specific conditions:
#' - **DATE_EQ**:
#'   - cell's value must be the same date as condition's value
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue`. 1 or more for filters on data source objects.
#' - **DATE_BEFORE**, **DATE_AFTER**
#'   - cell's value must be before (or after) the date of the condition's
#'   value
#'   - supported in: data validation, conditional formatting and filters
#'   - 1 `ConditionValue` (can be a *relative date*)
#' - **DATE_ON_OR_BEFORE**, **DATE_ON_OR_AFTER**
#'   - cell's value must be on or before (or after) the date of the condition's
#'   value
#'   - supported in data validation
#'   - 1 `ConditionValue` (can be a *relative date*)
#' - **DATE_BETWEEN**, **DATE_NOT_BETWEEN**
#'   - cell's value must be between (or outside of) two condition values.
#'   - supported in data validation
#'   - 2 `ConditionValue`s
#' - **DATE_IS_VALID**
#'   - cell's value must be a date
#'   - supported in data validation
#'   - NO `ConditionValue`
#'
#' # Non-specific conditions
#' - **ONE_OF_RANGE**
#'   - cell's value must be listed in the grid in the condition value's range.
#'   - supported in data validation
#'   - 1 `ConditionValue` that is a valid range in *A1 notation* (see: [get_A1_not()])
#' - **ONE_OF_LIST**
#'   - cell's value must be in the list of condition values
#'   - supported in data validation
#'   - Any number of `ConditionValue`s
#' - **BLANK**, **NOT_BLANK**
#'   - cell's value must be empty (or not empty)
#'   - supported in conditional formatting and filters
#'   - NO `ConditionValue`
#' - **CUSTOM_FORMULA**
#'   - formula in condition value must evaluate to `TRUE`
#'   - supported in data validation, conditional formatting and filters only
#'   in `GRID` sheets
#'   - 1 `ConditionValue` with formula
#' - **BOOLEAN**
#'   - the cell's value must be TRUE/FALSE or in list of condition values
#'   - supported in data validation
#'   - with NO `ConditionValue`s: value must be TRUE/FALSE and TRUE renders
#'   as checked, FALSE as unchecked
#'   - 1 `ConditionValue`: cell will render as checked when it contains that value
#'   and unchecked when blank
#'   - 2 `ConditionValue`s: cell will render as checked when it contains first
#'   value and unchecked when contains second
#' @export

BooleanCondition <- function(
    type = c("NUMBER_GREATER", "NUMBER_GREATER_THAN_EQ", "NUMBER_LESS",
             "NUMBER_LESS_THAN_EQ", "NUMBER_EQ", "NUMBER_NOT_EQ", "NUMBER_BETWEEN",
             "NUMBER_BETWEEN", "NUMBER_NOT_BETWEEN", "TEXT_CONTAINS",
             "TEXT_NOT_CONTAINS", "TEXT_STARTS_WITH", "TEXT_ENDS_WITH", "TEXT_EQ",
             "TEXT_IS_EMAIL", "DATE_EQ", "DATE_BEFORE", "DATE_AFTER", "DATE_ON_OR_BEFORE",
             "DATE_ON_OR_AFTER", "DATE_BETWEEN", "DATE_NOT_BETWEEN", "DATE_IS_VALID",
             "ONE_OF_RANGE", "ONE_OF_LIST", "BLANK", "NOT_BLANK", "CUSTOM_FORMULA",
             "BOOLEAN", "TEXT_NOT_EQ", "DATE_NOT_EQ"),
    values = NULL) {

  values <- nest_if_class(values, "ConditionValue") |>
    check_if_all_class("ConditionValue")

  type <- rlang::arg_match(type)

  out <- list() |>
    append_cond(type) |>
    append_cond(values) |>
    dgs4_class("BooleanCondition")

  return(out)

}

#' @rdname BooleanCondition
#' @param x any R object
#' @export
is.BooleanCondition <- function(x) {
  inherits(x, "BooleanCondition")
}

#' @title Condition Value
#' @description Value of the condition. Used in [BooleanCondition()]. Only
#' one of arguments below can be used.
#' @param userEnteredValue Value this condition is based on. Should be used
#' for most condition types. Formulas are supported and must begin with
#' `=` or `+`
#' @param relativeDate Date relative to the current date. Can be used only by
#' condition types: `DATE_BEFORE`, `DATE_AFTER`, `DATE_ON_OR_BEFORE`, `DATE_ON_OR_AFTER`.
#' Can be one of: `"PAST_YEAR"`, `"PAST_MONTH"`, `"PAST_WEEK"`, `"YESTERDAY"`,
#' `"TODAY"`, `"TOMORROW"`
#' @export
ConditionValue <- function(
    userEnteredValue = NULL,
    relativeDate = NULL) {

  null_args <- vapply(list(userEnteredValue, relativeDate), is.null, logical(1))

  if (sum(null_args) != 1)
    dgs4_error("Exactly one of {.arg userEnteredValue} or {.arg relativeDate} needs to be provided.")

  if (!is.null(userEnteredValue)) {

    obj <- check_if_type(userEnteredValue, "character")

  } else {

    obj <- check_if_options(relativeDate, "PAST_YEAR", "PAST_MONTH", "PAST_WEEK",
                            "YESTERDAY", "TODAY", "TOMORROW")
    attr(obj, "relativeDate") <- TRUE

  }

  obj <- dgs4_class(obj, "ConditionValue")

  return(obj)

}

#' @rdname ConditionValue
#' @param x any R object
#' @export
is.ConditionValue <- function(x) {
  inherits(x, "ConditionValue")
}
