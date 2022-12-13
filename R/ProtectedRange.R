#' @title Editors
#' @description Object declaring the editors of [ProtectedRange]
#' @param users The email addresses of users with edit access to the
#' protected range.
#' @param groups The email addresses of groups with edit access to the
#' protected range.
#' @param domainUsersCanEdit `TRUE` if anyone in the document's domain has edit
#' access to the protected range. Domain protection is only supported on
#' documents within a domain.
#' @export
#' @return dgs4Obj of class `Editors`
Editors <- function(users = NULL,
                    groups = NULL,
                    domainUsersCanEdit = NULL) {

  obj <- list() |>
    append_cond(users, class = "character", dgs4_class = FALSE) |>
    append_cond(users, class = "character", dgs4_class = FALSE) |>
    append_cond(domainUsersCanEdit, type = "logical") |>
    dgs4_class("Editors")

  return(obj)

}

#' @rdname Editors
#' @param x any R object
#' @export
is.Editors <- function(x) {
  is.dgs4_class(x, "Editors")
}

#' @title ProtectedRange
#' @description
#' Object declaring a protected range on a sheet, or a protected sheet (with unbounded `range`).
#' ProtectedRange can be declared either on the basis of `range` or `namedRangeId`.
#' If describing a protected sheet, `uprotectedRanges` can also be specified.
#' @param range object of class [GridRange]. The range that is being protected.
#' The range may be fully unbounded, in which case this is considered a protected sheet.
#' @param namedRangeId ID of the [NamedRange] this protected range is backed by.
#' @param description The description of this protected range.
#' @param warningOnly `TRUE` if this protected range will show a warning when editing.
#' @param unprotectedRanges object of class [GridRange] or list of such objects.
#' Unprotected ranges within a protected sheet (fully unbounded `range`)
#' @param editors object of class [Editors]. The users and groups with edit
#' access to the protected range. This field is only visible to users with
#' edit access to the protected range and the document. Editors are not
#' supported with `warningOnly` protection.
#' @param protectedRangeId **READ ONLY** The ID of the protected range.
#' @param requestingUserCanEdit **READ ONLY** `TRUE` if the user who requested this protected
#' range can edit the protected area.
#' @section warningOnly:
#' Warning-based protection means that every user can edit data in the protected range,
#' except editing will prompt a warning asking the user to confirm the edit.
#'
#' When writing: if `warningOnly` is true, then `editors` is ignored.
#' Additionally, if this field is changed from `TRUE` to `FALSE` and the
#' `editors` field is not set (nor included in the field mask), then the
#' `editors` will be set to all the editors in the document.
#'
#' @export
#' @return dgs4Obj of class `ProtectedRange`
ProtectedRange <- function(
    range = NULL,
    namedRangeId = NULL,
    description = NULL,
    warningOnly = NULL,
    unprotectedRanges = NULL,
    editors = NULL,
    protectedRangeId = NULL,
    requestingUserCanEdit = NULL) {

  ranges_specified <- vapply(list(range, namedRangeId),
                             is.null,
                             logical(1))

  if (sum(ranged_specified) != 1)
    dgs4_error("Exactly one of {.arg range} or {.arg namedRangeId} needs to be specified")

  unprotectedRanges <- nest_if_class(unprotectedRanges, "GridRange") |>
    check_if_all_class("GridRange")

  obj <- list() |>
    append_cond(range, class = "GridRange") |>
    append_cond(namedRangeId, type = "integer") |>
    append_cond(description, type = "character") |>
    append_cond(warningOnly, type = "logical") |>
    append_cond(unprotectedRanges) |>
    append_cond(editors, class = "Editors") |>
    append_cond(protectedRangeId, type = "integer") |>
    append_cond(requestingUserCanEdit, type = "logical") |>
    dgs4_class("ProtectedRange")

  return(obj)

}

#' @rdname ProtectedRange
#' @param x any R object
#' @export
is.ProtectedRange <- function(x) {
  is.dgs4_class(x, "ProtectedRange")
}

