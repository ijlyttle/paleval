#' ggplot for HCL data on series of hex-colors
#'
#' @inherit pev_gg_hcl_bloom params return
#'
#' @examples
#'   pev_gg_hcl_plane(pev_data_hcl("Greens"))
#' @export
#'
pev_gg_hcl_plane <- function(data_hcl, data_hcl_ref = NULL) {

  # preprocess data
  data_pre <- gg_hcl_preprocess(data_hcl, data_hcl_ref)

  # get max-chroma data (internal function)
  data_max <- data_hcl_chroma_max(data_hcl)

  g <-
    ggplot2::ggplot(
      data_pre,
      ggplot2::aes_string(
        x = "chroma",
        y = "luminance",
        color = "hex",
        shape = "annotation"
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = data_max, shape = 18, size = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0, NA)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_manual(
      values = c(
        "inside RGB-space" = 19,
        "at RGB boundary" = 17 ,
        "reference" = 4
      )
    ) +
    ggplot2::facet_wrap(
      facets = "cvd",
      ncol = 2,
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      strip.position = "right"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_line(colour = "grey85"),
      panel.grid.minor = ggplot2::element_line(colour = "grey90")
    )

  g
}

data_hcl_chroma_max <- function(data_hcl) {

  # get cvd status
  include_cvd <- !identical(unique(data_hcl$cvd), "none")

  # remove all but cvd:none
  hue <- data_hcl$hue[data_hcl$cvd == "none"]
  luminance <- data_hcl$luminance[data_hcl$cvd == "none"]

  # create interpolation-functions for hue and luminance
  x <- seq(0, 1, length.out = length(hue))
  f_hue <- stats::approxfun(x, hue)
  f_luminance <- stats::approxfun(x, luminance)

  x_new <- seq(0, 1, length.out = 101)
  hue_new <- f_hue(x_new)
  luminance_new <- f_luminance(x_new)
  chroma_new <- colorspace::max_chroma(hue_new, luminance_new)
  hex_new <- colorspace::hex(
    colorspace::polarLUV(
      L = luminance_new,
      C = chroma_new,
      H = hue_new
    ),
    fixup = TRUE
  )

  data_max <- pev_data_hcl(hex_new, include_cvd = include_cvd)

  data_max$annotation <- NA_character_
  data_max$cvd <- factor(data_max$cvd, levels = get_cvd())

  data_max
}
