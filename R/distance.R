#' Get perceptual-distance of continuous-palette from reference color
#'
#' @inheritParams pev_fcont
#' @inheritParams pev_data_separation
#' @param hex_ref `character` single hex-colors to act as the reference, default value
#'   is the beginning of the range of the continuous-palette, i.e. at `x = 0`.
#' @param n_panel `numeric` number of panels into which to discretize the
#'   domain (0 to 1) of the continous-palette.
#'
#'
#' @return `data.frame` with variables `cvd`, `x`, `hex`, `hex_ref`, `distance`
#' @examples
#'   pev_data_distance("Purple-Green")
#' @export
#'
pev_data_distance <- function(.fcont, hex_ref = NULL, n_panel = 40,
                              include_cvd = TRUE) {

  # coerce to continuous-palette
  .fcont <- pev_fcont(.fcont)
  hex_ref <- hex_ref %||% .fcont(0)

  assertthat::assert_that(
    is_hexcolor(hex_ref),
    assertthat::is.scalar(hex_ref)
  )

  list_cvd <- get_cvd(include_cvd)
  list_fcvd <-
    purrr::map(
      list_cvd,
      function(x) {
        pev_fcont_cvd(.fcont, x)
      }
    )

  list_x <- purrr::map(list_cvd, ~seq(0, 1, length.out = n_panel + 1))
  list_hex <- purrr::map2(list_fcvd, list_x, purrr::exec)
  list_hex_ref <-
    purrr::map(
      list_cvd,
      function(cvd, hex) {
        .f <- .pev_cvd(cvd)
        .f(hex)
      },
      rep(hex_ref, n_panel + 1)
    )


  data_dist <-
    tibble::tibble(
      cvd = list_cvd,
      x = list_x,
      hex = list_hex,
      hex_ref = list_hex_ref
    )

  data_dist <- tidyr::unnest(data_dist, c("cvd", "x", "hex", "hex_ref"))

  data_dist$distance <- pev_hex_distance(data_dist$hex, data_dist$hex_ref)

  data_dist
}

#' ggplot for perceptual-derivative of continuous-palette
#'
#' @param data_dist `data.frame`, created using [pev_data_distance()].
#'
#' @return `ggplot` object
#' @examples
#'   data_dist <- pev_data_distance("Purple-Green")
#'   pev_gg_distance(data_dist)
#' @export
#'
pev_gg_distance <- function(data_dist) {

  data_dist$cvd <-
    factor(data_dist$cvd, levels = c("none", "deutan", "protan", "tritan"))

  g <-
    ggplot2::ggplot(
      data_dist,
      ggplot2::aes_string(x = "x", y = "distance")
    ) +
    ggplot2::geom_point(ggplot2::aes_string(color = "hex")) +
    ggplot2::scale_color_identity() +
    ggplot2::ylim(0, NA) +
    ggplot2::labs(y = "percertual distance") +
    ggplot2::facet_grid(
      rows = "cvd",
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      scales = "fixed"
    ) +
    ggplot2::theme_light()

  g

}
