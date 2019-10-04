#' Get separation within discrete-palette
#'
#' @inherit pev_fdisc params
#' @param method `character` method to use for comparison,
#'   passed to `farver::compare_color()`.
#'   One of: `"euclidean"`, `"cie1976"`, `"cie94"`, `"cie2000"`, or `"cmc"`.
#' @param include_cvd `logical`, indicates to include data for
#'   for color-vision deficiency
#'
#' @seealso pev_gg_separation
#'
#' @return `tbl_df` with variables `cvd`, `color_a`, `color_b`, `difference`
#' @examples
#'   fcont <- pev_fcont("Dynamic")
#'   fdisc <- pev_fdisc(fcont, n = 7, method = "panel")
#'   pev_data_separation(fdisc)
#' @export
#'
pev_data_separation <- function(.fdisc, method = "cie2000", include_cvd = TRUE) {

  # coerce to fdisc
  .fdisc <- pev_fdisc(.fdisc)

  # set up cvd
  cvd <- c("none", "deutan", "protan", "tritan")

  if (!include_cvd) {
    cvd <- "none"
  }

  # make list of functions, palettes
  list_fdisc <- purrr::map(cvd, ~pev_fpal_cvd(.fdisc, .x))
  list_hex <- purrr::map(list_fdisc, pev_fpal_to_hex)

  data_cvd <- tibble::tibble(cvd = cvd, hex = list_hex)

  data_cvd$diff <- purrr::map(data_cvd$hex, .pev_data_separation, method = method)
  data_cvd$hex <- NULL

  data_cvd <- tidyr::unnest(data_cvd, cols = "diff")

  data_cvd
}

# implementation for single set of hex-colors
.pev_data_separation <- function(hex, method = "cie2000") {

  data <- tidyr::expand_grid(color_a = hex, color_b = hex)

  compare <- function(a, b) {

    rgb_a <- t(grDevices::col2rgb(a))
    rgb_b <- t(grDevices::col2rgb(b))

    farver::compare_colour(rgb_a, rgb_b, from_space = "rgb", method = method)
  }

  data$index_a <- as.integer(factor(data$color_a, levels = hex))
  data$difference <- purrr::map2_dbl(data$color_a, data$color_b, compare)

  data[, c("index_a", "color_a", "color_b", "difference")]
}

#' ggplot for perceptual-differences within palette
#'
#' @param data_sep `data.frame`, created using `pev_data_separation`.
#' @param ncol `numeric`, number of columns in the facet
#' @param height_tick `numeric`, height (units of `difference`) of the
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
      ggplot2::aes_string(x = "index_a", y = Inf, fill = "color_a"),
      stat = "identity",
      position = "identity",
      width = 0.3
    ) +
    ggplot2::geom_tile(
      ggplot2::aes_string(x = "index_a", y = "difference", fill = "color_b"),
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
      y = "difference"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  g
}


