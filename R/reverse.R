#' Reverse palette-function
#'
#' @inherit pev_fpal_cvd params return
#'
#' @examples
#'   # Create continuous diverging-palette function
#'   fcont_purple_green <- pev_fcont("Purple-Green")
#'   fcont_purple_green
#'
#'   # Create discrete diverging-palette function
#'   fdisc_purple_green <- pev_fdisc(fcont_purple_green, n = 11)
#'   fdisc_purple_green
#'
#'   # Simulate color-vision deficiency
#'   pev_fpal_reverse(fcont_purple_green)
#'   pev_fpal_reverse(fdisc_purple_green)
#'
#' @export
#'
pev_fpal_reverse <- function(.fpal, ...) {
  UseMethod("pev_fpal_reverse")
}

#' @rdname pev_fpal_reverse
#' @export
#'
pev_fpal_reverse.default <- function(.fpal, ...) {
  stop(
    glue::glue("No method for `pev_fcont` for class {class(.fpal)}"),
    call. = FALSE
  )
}

#' @rdname pev_fpal_reverse
#' @export
#'
pev_fpal_reverse.pev_fcont <- function(.fpal, ...) {
  pev_fcont_rescale(.fpal, limits = c(1, 0))
}

#' @rdname pev_fpal_reverse
#' @export
#'
pev_fpal_reverse.pev_fdisc <- function(.fpal, ...) {
  hex <- .fpal()

  pev_fdisc(rev(hex))
}
