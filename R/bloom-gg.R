#' ggplot for HCL data on series of hex-colors
#'
#' Create a plot like Tableau made [here](https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782).
#'
#' @param data_bloom `data.frame` created using `pev_data_bloom`.
#' @param label `logical` indicates to use axes labels, titles, etc.
#' @param max_chroma `numeric` upper-limit for the chroma scale
#'
#' @examples
#'   # without color-vision deficiency, with labels
#'   data_no_cvd <- pev_data_bloom("Viridis", include_cvd = FALSE)
#'   pev_gg_bloom(data_no_cvd, label = TRUE)
#'
#'   # without color-vision deficiency, without labels
#'   pev_data_bloom("Viridis") %>% pev_gg_bloom()
#' @export
#'
pev_gg_bloom <- function(data_bloom, label = FALSE, max_chroma = NULL) {

  data_bloom$cvd <- factor(data_bloom$cvd, levels = get_cvd())

  # make our ceiling for the chroma scale
  max_chroma <- max_chroma %||% ceiling(max(data_bloom$chroma) / 50) * 50

  # turn data into list keyed by `cvd`
  list_bloom <- by(data_bloom, data_bloom$cvd, identity)

  # drop NULL elements
  list_bloom <- purrr::compact(list_bloom)

  # is there only one?
  one_bloom <- identical(length(list_bloom), 1L)

  # get ggplot objects for target
  list_bloom_target <-
    purrr::map(
      list_bloom,
      pev_gg_bloom_target,
      label = label,
      max_chroma = max_chroma)

  # get ggplot objects for luminance
  list_bloom_luminance <-
    purrr::map(list_bloom, pev_gg_bloom_lum, label = label)

  # remove facet-labels from target-plots
  list_bloom_target <-
    purrr::map(
      list_bloom_target,
      function(g) {
        g <- g + ggplot2::theme(strip.text = ggplot2::element_blank())
      }
    )

  # blend into a big list
  list_bloom_all <- vector("list", length = length(list_bloom))
  for (i in seq_along(list_bloom_target)) {
    list_bloom_all[2 * i - 1] <- list_bloom_target[i]
    list_bloom_all[2 * i] <- list_bloom_luminance[i]
  }

  ncol <- ifelse(one_bloom, 2, 4)
  # compose plots
  compose <-
    cowplot::plot_grid(
      plotlist = list_bloom_all,
      ncol = ncol,
      rel_widths = rep(c(3, 1), times = ncol)
    )

  compose
}

#' @rdname pev_gg_bloom
#' @export
#'
pev_gg_bloom_target <- function(data_bloom, label = FALSE, max_chroma = 200) {

  data_bloom$cvd <- factor(data_bloom$cvd, levels = get_cvd())

  g <-
    ggplot2::ggplot(
      data_bloom,
      ggplot2::aes_string(x = "hue", y = "chroma")
    ) +
    ggplot2::geom_point(
      ggplot2::aes_string(color = "hex")
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 360),
      breaks = seq(0, 360, 60)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max_chroma),
      breaks = seq(0, 200, 50)
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::coord_polar(start = -pi/2, direction = -1) +
    ggplot2::facet_wrap(
      facets = "cvd",
      ncol = 2,
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      strip.position = "right"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey85"),
      panel.grid.minor = ggplot2::element_line(colour = "grey90")
    )

  if (!label) {
    g <-
      g +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      )
  }

  g
}

#' @rdname pev_gg_bloom
#' @export
#'
pev_gg_bloom_lum <- function(data_bloom, label = FALSE) {

  data_bloom$cvd <- factor(data_bloom$cvd, levels = get_cvd())

  g <-
    ggplot2::ggplot(
      data_bloom,
      ggplot2::aes_string(x = "x", y = "luminance")
    ) +
    ggplot2::geom_segment(
      ggplot2::aes_string(
        x = "x - 0.25",
        xend = "x + 0.25",
        yend = "luminance",
        color = "hex"
      )
    ) +
    ggplot2::scale_x_continuous(limits = c(-0.25, 1.25), breaks = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 100)) +
    ggplot2::scale_color_identity() +
    ggplot2::coord_fixed(0.075) +
    ggplot2::facet_wrap(
      facets = "cvd",
      ncol = 2,
      labeller = ggplot2::labeller(.rows = ggplot2::label_both),
      strip.position = "right"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey85"),
      panel.grid.minor = ggplot2::element_line(colour = "grey90")
    )

  if (!label) {
    g <-
      g +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank()
      )
  }

  g
}


