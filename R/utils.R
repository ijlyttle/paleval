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

extent <- function(x, increment = 50) {
  ceiling(max(x) / increment) * increment
}

gg_hcl_preprocess <- function(data_hcl, data_hcl_ref) {

  names <-
    c("cvd", "x", "hex", "hue", "chroma", "luminance", "annotation")

  data_hcl$annotation <-
    ifelse(
      data_hcl$is_rgb_limit,
      "at RGB boundary",
      "inside RGB-space"
    )

  if (!is.null(data_hcl_ref)) {
    data_hcl_ref$x <- data_hcl_ref$x_nearest
    data_hcl_ref$hex <- data_hcl_ref$hex_ref
    data_hcl_ref$hue <- data_hcl_ref$hue_ref
    data_hcl_ref$chroma <- data_hcl_ref$chroma_ref
    data_hcl_ref$luminance <-data_hcl_ref$luminance_ref
    data_hcl_ref$annotation <- "reference"
  }

  data_pre <- rbind(data_hcl[, names], data_hcl_ref[, names])
  data_pre$cvd <- factor(data_pre$cvd, levels = get_cvd())

  data_pre
}
