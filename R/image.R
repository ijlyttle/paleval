img_cont_ramp <- function(pev_fcont, width = 512, height = 32) {

  val <- function(i) {
    (i - 1) / (width - 1)
  }

  dat <- matrix(rep(val(1:width), times = height), ncol = height, nrow = width)

  colors <- pev_fcont(seq(0, 1, by = 1 / (width - 1)))

  op <- graphics::par(mar = rep(0, 4))
  graphics::image(dat, col = colors, useRaster = TRUE)
  graphics::par(op)

  invisible(NULL)
}
