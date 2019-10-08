#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

`%||%` <- rlang::`%||%`

get_cvd <- function(include_cvd = TRUE) {
  # set up cvd
  cvd <- c("none", "deutan", "protan", "tritan")

  if (!include_cvd) {
    cvd <- "none"
  }

  cvd
}

pev_nmax_display <- function(.fpal) {

  if (inherits(.fpal, "pev_funbounded") || inherits(.fpal, "pev_fcont")) {
    n <- 11
  }

  if (inherits(.fpal, "pev_fbounded")) {
    n <- pev_nmax(.fpal)
  }

  n
}
