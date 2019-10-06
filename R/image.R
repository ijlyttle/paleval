img_cont_ramp <- function(pev_fcont, width = 512, height = 32) {

  val <- function(i) {
    (i - 1) / (width - 1)
  }

  dat <- matrix(rep(val(1:width), times = height), ncol = height, nrow = width)

  colors <- pev_fcont(seq(0, 1, by = 1 / (width - 1)))

  op <- graphics::par(mar = rep(0.5, 4), xaxt = "n", yaxt = "n", bty = "n")
  graphics::image(dat, col = colors, useRaster = TRUE)
  graphics::par(op)

  invisible(NULL)
}

img_disc <- function(pev_fdisc, n = NULL, width_panel = 32, width_gap = 8, height = 32) {

  n <- n %||% pev_nmax_display(pev_fdisc)

  colors <- pev_fdisc(n)

  ncol <- height
  nrow <- n * width_panel + (n - 1) * width_gap

  strip <- function(i) {
    rep(i, width_panel)
  }

  gap <- rep(NA_real_, width_gap)

  strip_gap <- function(i) {
    c(strip(i), gap)
  }

  strip_dat <- purrr::reduce(purrr::map(seq(1, n - 1),  strip_gap), c)

  dat <- matrix(
    rep(c(strip_dat, strip(n)), times = ncol),
    ncol = ncol,
    nrow = nrow
  )

  op <- graphics::par(mar = rep(0.5, 4), xaxt = "n", yaxt = "n", bty = "n")
  graphics::image(dat, col = colors, useRaster = TRUE)
  graphics::par(op)

  invisible(NULL)
}
