#' ggplot for HCL data on series of hex-colors
#'
#' @inherit pev_gg_hcl_bloom params return
#' @examples
#'   pev_gg_hcl_spectrum(pev_data_hcl("Greens"))
#' @export
#'
pev_gg_hcl_spectrum <- function(data_hcl, data_hcl_ref = NULL) {

  # this is a tool to "capture" reference-colors in a palette, so the
  # default is to show for non-cvd only

  # preprocess data
  data_pre <- gg_hcl_preprocess(data_hcl, data_hcl_ref)

  data_pre <- data_pre[data_pre$cvd == "none", ]

  base_plot <-
    ggplot2::ggplot(
      data_pre,
      ggplot2::aes_string(x = "x", color = "hex", shape = "annotation")
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_manual(
      values = c(
        "inside RGB-space" = 19,
        "at RGB boundary" = 17 ,
        "reference" = 4
      )
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "none",
      panel.grid.major = ggplot2::element_line(colour = "grey85"),
      panel.grid.minor = ggplot2::element_line(colour = "grey90")
    )

  hue_plot <-
    base_plot +
    ggplot2::geom_point(ggplot2::aes_string(y = "hue"))

  chroma_plot <-
    base_plot +
    ggplot2::geom_point(ggplot2::aes_string(y = "chroma"))

  luminance_plot <-
    base_plot +
    ggplot2::geom_point(ggplot2::aes_string(y = "luminance"))

  # extract legend
  legend <- cowplot::get_legend(
    hue_plot + ggplot2::theme(legend.position = "bottom")
  )

  # compose
  cowplot::plot_grid(
    hue_plot,
    chroma_plot,
    luminance_plot,
    legend,
    ncol = 1,
    rel_heights = c(1, 1, 1, 0.2)
  )
}
