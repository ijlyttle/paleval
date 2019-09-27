#' Get perceptual-differences within palette
#'
#' @param pal `character` (coerceable to vector of hex-codes), describes a
#'   discrete color palette. Interpolation within a palette is not required to
#'   be a well-formed idea.
#' @param method `character` method to use for comparison,
#'   passed to `farver::compare_color()`.
#'   One of: `"euclidean"`, `"cie1976"`, `"cie94"`, `"cie2000"`, or `"cmc"`.
#' @param include_cvd `logical`, indicates to include differences
#'   for color-vision deficiency
#'
#' @seealso pev_gg_separation
#'
#' @return `tbl_df` with variables `cvd`, `color_a`, `color_b`, `difference`
#' @examples
#'   library("colorspace")
#'   pal <- qualitative_hcl(n = 5)
#'   pev_data_separation(pal)
#' @export
#'
pev_data_separation <- function(pal, method = "cie2000", include_cvd = TRUE) {

  data_cvd <- tibble::tibble(
    cvd = "none",
    pal = list(pal)
  )

  if (include_cvd) {
    data_cvd <- tibble::tibble(
      cvd = c("none", "deutan", "protan", "tritan"),
      pal = list(
        pal,
        colorspace::deutan(pal),
        colorspace::protan(pal),
        colorspace::tritan(pal)
      )
    )
  }

  data_cvd$diff <- purrr::map(data_cvd$pal, .pev_data_separation, method = method)
  data_cvd$pal <- NULL

  data_cvd <- tidyr::unnest(data_cvd, cols = "diff")

  data_cvd
}

# implementation for single palette
.pev_data_separation <- function(pal, method = "cie2000") {

  # coerce to hexcolor
  pal <- as_hexcolor(pal)

  data <- tidyr::expand_grid(color_a = pal, color_b = pal)

  compare <- function(a, b) {

    rgb_a <- t(grDevices::col2rgb(a))
    rgb_b <- t(grDevices::col2rgb(b))

    farver::compare_colour(rgb_a, rgb_b, from_space = "rgb", method = method)
  }

  data$difference <- purrr::map2_dbl(data$color_a, data$color_b, compare)

  data
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
#'   library("colorspace")
#'   pal <- qualitative_hcl(n = 5)
#'   data_sep <- pev_data_separation(pal)
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
      ggplot2::aes_string(x = "color_a", y = Inf, fill = "color_a"),
      stat = "identity",
      position = "identity",
      width = 0.3
    ) +
    ggplot2::geom_tile(
      ggplot2::aes_string(x = "color_a", y = "difference", fill = "color_b"),
      width = 0.6,
      height = height_tick
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_wrap(
      "cvd",
      ncol = ncol,
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      scales = "free_x",
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


