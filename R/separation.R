#' Get perceptual-differences within palette
#'
#' @param pal `character` (coerceable to vector of hex-codes), describes a
#'   discrete color palette. Interpolation within a palette is not required to
#'   be a well-formed idea.
#'
#' @return `tbl_df` with variables `color_a`, `color_b`, `difference`
#' @export
#'
pev_data_separation <- function(pal) {

  # coerce to hexcolor
  pal <- as_hexcolor(pal)

  data <- tidyr::expand_grid(color_a = pal, color_b = pal)

  compare <- function(a, b) {

    rgb_a <- t(grDevices::col2rgb(a))
    rgb_b <- t(grDevices::col2rgb(b))

    farver::compare_colour(rgb_a, rgb_b, from_space = "rgb", method = "cie2000")
  }

  data$difference <- purrr::map2_dbl(data$color_a, data$color_b, compare)

  data
}

pev_gg_separation <- function(data_pal) {

  g <-
    ggplot2::ggplot(data_pal) +
    ggplot2::geom_bar(
      ggplot2::aes(x = color_a, y = 100, fill = color_a),
      stat = "identity",
      position = "identity",
      width = 0.3
    ) +
    scale_fill_identity()

  g
}
