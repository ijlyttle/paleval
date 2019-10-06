#' Get separation within discrete-palette
#'
#' @inherit pev_fdisc params
#' @inherit pev_hex_distance params
#' @param n `integer` number of colors to use for unbounded palette-functions
#' @param include_cvd `logical`, indicates to include data for
#'   for color-vision deficiency
#'
#' @seealso pev_gg_separation
#'
#' @return `tbl_df` with variables `cvd`, `hex_a`, `hex_b`, `distance`
#' @examples
#'   fcont <- pev_fcont("Dynamic")
#'   fdisc <- pev_fdisc(fcont, n = 7, method = "panel")
#'   pev_data_separation(fdisc)
#' @export
#'
pev_data_separation <- function(.fdisc, n = NULL, method = "cie2000", include_cvd = TRUE) {

  # coerce to fdisc
  .fdisc <- pev_fdisc(.fdisc)

  # set up cvd
  cvd <- c("none", "deutan", "protan", "tritan")

  if (!include_cvd) {
    cvd <- "none"
  }

  # determine n
  n <- n %||% pev_nmax_display(.fdisc)

  # make list of functions, palettes
  list_hex <- purrr::map(cvd, ~pev_fdisc_cvd(.fdisc, type = .x)(n))

  data_cvd <- tibble::tibble(cvd = cvd, hex = list_hex)

  data_cvd$diff <- purrr::map(data_cvd$hex, .pev_data_separation, method = method)
  data_cvd$hex <- NULL

  data_cvd <- tidyr::unnest(data_cvd, cols = "diff")

  data_cvd
}

# implementation for single set of hex-colors
.pev_data_separation <- function(hex, method = "cie2000") {

  data <- tidyr::expand_grid(hex = hex, hex_ref = hex)

  data$i <- as.integer(factor(data$hex, levels = hex))
  data$distance <-
    purrr::map2_dbl(data$hex, data$hex_ref, pev_hex_distance, method = method)

  data[, c("i", "hex", "hex_ref", "distance")]
}

#' ggplot for perceptual distance within palette
#'
#' @param data_sep `data.frame`, created using `pev_data_separation`.
#' @param ncol `numeric`, number of columns in the facet
#' @param height_tick `numeric`, height (units of `distance`) of the
#'   cross-wise ticks
#'
#' @return `ggplot` object
#' @examples
#'   fcont <- pev_fcont("Dynamic")
#'   fdisc <- pev_fdisc(fcont, n = 7, method = "panel")
#'   data_sep <- pev_data_separation(fdisc)
#'   pev_gg_separation(data_sep)
#' @export
#'
pev_gg_separation <- function(data_sep, ncol = 2, height_tick = 1) {

  data_sep$cvd <-
    factor(
      data_sep$cvd,
      levels = c("none", "deutan", "protan", "tritan")
    )

  g <-
    ggplot2::ggplot(data_sep) +
    ggplot2::geom_bar(
      ggplot2::aes_string(x = "i", y = Inf, fill = "hex"),
      stat = "identity",
      position = "identity",
      width = 0.3
    ) +
    ggplot2::geom_tile(
      ggplot2::aes_string(x = "i", y = "distance", fill = "hex_ref"),
      width = 0.6,
      height = height_tick
    ) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_wrap(
      "cvd",
      ncol = ncol,
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      strip.position = "right"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "distance"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  g
}


