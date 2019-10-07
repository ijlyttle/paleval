#' Get perceptual-derivative of continuous-palette
#'
#' @inheritParams pev_fcont
#' @inheritParams pev_data_separation
#' @param n_panel `numeric`, number of panels into which to discretize the
#'   palette-function.
#'
#' @return `data.frame` with variables `cvd`, `x`, `hex`, `d_distance_d_x`
#' @examples
#'   pev_data_derivative("Viridis")
#' @export
#'
pev_data_derivative <- function(.fcont, n_panel = 40, method = "cie2000",
                                include_cvd = TRUE) {

  # coerce to continuous-palette
  .fcont <- pev_fcont(.fcont)

  # discretize
  x <- seq(0, 1, by = 1 / n_panel)

  # set up cvd
  cvd <- c("none", "deutan", "protan", "tritan")

  if (!include_cvd) {
    cvd <- "none"
  }

  data_cvd <- tibble::tibble(
    cvd = cvd,
    x = rep(list(x), length(cvd))
  )

  data_cvd$hex <- purrr::map(data_cvd$cvd, ~pev_fcont_cvd(.fcont, .x)(x))
  data_cvd$d_distance_d_x <-
    purrr::map(data_cvd$hex, pev_hex_derivative, method = method)

  data_cvd <- tidyr::unnest(data_cvd, cols = c("x", "hex", "d_distance_d_x"))

  data_cvd
}

#' ggplot for perceptual-derivative of continuous-palette
#'
#' @param data_drv `data.frame`, created using [pev_data_derivative()].
#'
#' @return `ggplot` object
#' @examples
#'   data_drv <- pev_data_derivative("Viridis")
#'   pev_gg_derivative(data_drv)
#' @export
#'
pev_gg_derivative <- function(data_drv) {

  data_drv$cvd <-
    factor(data_drv$cvd, levels = c("none", "deutan", "protan", "tritan"))

  g <-
    ggplot2::ggplot(
      data_drv,
      ggplot2::aes_string(x = "x", y = "d_distance_d_x")
    ) +
    ggplot2::geom_point(ggplot2::aes_string(color = "hex")) +
    ggplot2::scale_color_identity() +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_grid(
      rows = "cvd",
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      scales = "fixed"
    ) +
    ggplot2::theme_light()

  g
}
