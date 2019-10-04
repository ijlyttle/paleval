#' Convert to hex-colors
#'
#' @inherit pev_fpal_cvd params
#' @inherit pev_fdisc params
#'
#' @return `character` vector of hex-colors
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
#'   # Convert to hex-colors
#'   pev_fpal_to_hex(fcont_purple_green, n = 11)
#'   pev_fpal_to_hex(fdisc_purple_green)
#'
#' @export
#'
pev_fpal_to_hex <- function(.fpal, ...) {
  UseMethod("pev_fpal_to_hex")
}

#' @rdname pev_fpal_to_hex
#' @export
#'
pev_fpal_to_hex.default <- function(.fpal, ...) {
  stop(
    glue::glue("No method for `pev_fpal_to_hex` for class {class(.fpal)}"),
    call. = FALSE
  )
}

#' @rdname pev_fpal_to_hex
#' @export
#'
pev_fpal_to_hex.pev_fcont <- function(.fpal, n = 11,
                                      method = c("post", "panel"), ...) {

  fdisc <- pev_fdisc(.fpal, n = n, method = method)

  pev_fpal_to_hex(fdisc)
}

#' @rdname pev_fpal_to_hex
#' @export
#'
pev_fpal_to_hex.pev_fdisc <- function(.fpal, ...) {
  .fpal()
}

