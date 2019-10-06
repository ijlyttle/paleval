#' Reverse palette-function
#'
#' @inherit pev_fcont_cvd params return
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
#'   # Reverse the palettes
#'   pev_fcont_reverse(fcont_purple_green)
#'   pev_fdisc_reverse(fdisc_purple_green)
#'
#' @export
#'
pev_fcont_reverse <- function(.fcont, ...) {
  UseMethod("pev_fcont_reverse")
}

#' @rdname pev_fcont_reverse
#' @export
#'
pev_fcont_reverse.default <- function(.fcont, ...) {
  stop(
    glue::glue("No method for `pev_fcont_reverse` for class {class(.fcont)}"),
    call. = FALSE
  )
}

#' @rdname pev_fcont_reverse
#' @export
#'
pev_fcont_reverse.pev_fcont <- function(.fcont, ...) {
  pev_fcont_rescale(.fcont, limits = c(1, 0))
}

#' @rdname pev_fcont_reverse
#' @export
#'
pev_fdisc_reverse <- function(.fdisc, ...) {
  UseMethod("pev_fdisc_reverse")
}

#' @rdname pev_fcont_reverse
#' @export
#'
pev_fdisc_reverse.default <- function(.fdisc, ...) {
  stop(
    glue::glue("No method for `pev_fdisc_reverse` for class {class(.fdisc)}"),
    call. = FALSE
  )
}

#' @rdname pev_fcont_reverse
#' @export
#'
pev_fdisc_reverse.pev_funbounded <- function(.fdisc, ...) {

  f <- function(n) {
    rev(.fdisc(n))
  }

  new_pev_funbounded(f)
}

#' @rdname pev_fcont_reverse
#' @export
#'
pev_fdisc_reverse.pev_fbounded <- function(.fdisc, ...) {

  f <- function(n) {
    rev(.fdisc(n))
  }

  attr(f, "n_max") <- attr(.fdisc, "n_max")

  new_pev_fbounded(f)
}
