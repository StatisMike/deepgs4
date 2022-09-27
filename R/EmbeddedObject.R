#' @title Overlay Position
#' @description The location an object is overlaid on top of a grid.
#' @param anchorCell object of class [GridCoordinate] - cell that the object
#' is anchored to
#' @param offsetXPixels,offsetYPixels horizontal and vertical offset in pixels
#' from the anchor cell
#' @param widthPixels,heightPixels width and height of the object in pixels
#' @export
OverlayPosition <- function(
    anchorCell,
    offsetXPixels = NULL,
    offsetYPixels = NULL,
    widthPixels = 600,
    heightPixels = 371){

  out <- list() |>
    append_cond(anchorCell, class = "GridCoordinate", skip_null = FALSE) |>
    append_cond(offsetXPixels, type = "integer") |>
    append_cond(offsetYPixels, type = "integer") |>
    append_cond(widthPixels, type = "integer") |>
    append_cond(heightPixels, type = "integer") |>
    deepgs_class("OverlayPosition")

  return(out)

}

#' @rdname OverlayPosition
#' @param x any R object
#' @export
is.OverlayPosition <- function(x) {
  inherits(x, "OverlayPosition")
}

#' @rdname OverlayPosition
#' @param obj list produced by `deepgs_listinize()`
#' @param sheetProperties optional `SheetProperties` object to get additional
#' data during read from API
#' @export
gen_OverlayPosition <- function(obj, sheetProperties = NULL) {

  obj$anchorCell <- gen_GridCoordinate(obj$anchorCell,
                                       sheetProperties = sheetProperties)

  do.call(OverlayPosition,
          args = obj)

}

#' @title EmbeddedObjectPosition
#' @description Specification of position for embedded objects (eg.
#' charts and slicers). Out of three arguments, only one can be provided. They
#' are interpreted in order.
#' @param overlayPosition object of class [OverlayPosition] declaring position
#' of EmbeddedObject in existing sheet
#' @param sheetId the ID of sheet this object is in if EmbeddedObject is in
#' its own sheet during read or update. When creating new object, it specifies
#' ID of new sheet that will be created.
#' @param newSheet TRUE if the object is to be put on a new sheet. Its ID is
#' chosen for you. Used only when creating new object.
#' @export
#' @return EmbeddedObjectPosition
EmbeddedObjectPosition <- function(
    overlayPosition = NULL,
    sheetId = NULL,
    newSheet = TRUE) {

  if (!is.null(overlayPosition)) {
    out <- list() |>
      append_cond(overlayPosition, class = "OverlayPosition", skip_null = FALSE) |>
      deepgs_class("EmbeddedObjectPosition")

    return(out)
  }

  if (!is.null(sheetId)) {
    out <- list() |>
      append_cond(sheetId, type = "integer", skip_null = FALSE) |>
      deepgs_class("EmbeddedObjectPosition")

    return(out)
  }

  out <- list() |>
    append_cond(newSheet, type = "logical", skip_null = FALSE) |>
    deepgs_class("EmbeddedObjectPosition")

  if (!isTRUE(out$newSheet))
    deepgs_error("{.arg newSheet} needs to be {.val TRUE}.",
                 class = "WrongArgError")

  return(out)

}

#' @rdname EmbeddedObjectPosition
#' @param x any R object
#' @export
is.EmbeddedObjectPosition <- function(x) {
  inherits(x, "EmbeddedObjectPosition")
}

#' @rdname EmbeddedObjectPosition
#' @param obj list produced by `deepgs_listinize()`
#' @param sheetProperties optional `SheetProperties` object to get additional
#' data during read from API
#' @export
gen_EmbeddedObjectPosition <- function(obj, sheetProperties = NULL) {

  if (!is.null(obj$overlayPosition))
    obj$overlayPosition <- gen_OverlayPosition(obj$overlayPosition,
                                               sheetProperties = sheetProperties)

  do.call(OverlayPosition,
          args = obj)

}
