#' Create HCL parameter-set
#'
#' This follows the colorspace scheme.
#'
#' @inheritParams colorspace::sequential_hcl
#'
#' @return Object with S3 class `pev_hcl_param`
#' @examples
#'  pev_hcl_param(h1 = 0, h2 = 360, c1 = 60, l1 = 60)
#' @export
#'
pev_hcl_param <- function(h1, h2 = h1, c1, c2 = c1, l1, l2 = l1,
                          p1 = 1, p2 = 1, cmax = c1, fixup = TRUE) {

  pev_hcl_param = structure(
    list(
      h1 = h1,
      h2 = h2,
      c1 = c1,
      cmax = cmax,
      c2 = c2,
      l1 = l1,
      l2 = l2,
      p1 = p1,
      p2 = p2,
      fixup = fixup
    ),
    class = "pev_hcl_param"
  )

}

print.pev_hcl_param <- function(x, ...) {
  f <- function(x) {
    sprintf("% 7.2f", x)
  }

  print(
    glue::glue(
      "HCL Parameters (colorspace)",
      "Hue:\t\tstart: {f(x$h1)}\tend: {f(x$h2)}",
      "Chroma:\t\tstart: {f(x$c1)}\tend: {f(x$c2)}\tmax: {f(x$cmax)}",
      "Luminance:\tstart: {f(x$l1)}\tend: {f(x$l2)}",
      "Exponent:\t  [1]: {f(x$p1)}\t[2]: {f(x$p2)}",
      .sep = "\n"
    )

  )


  invisible(x)
}

#' Create list of HCL parameter-sets
#'
#' @param hcl_palettes Object with S3 class `hcl_palettes`,
#'   created using [colorspace::hcl_palettes()].
#'
#' @return Named list, elements are objects wiht S3 class `pev_hcl_param`
#'
pev_map_hcl_param <- function(hcl_palettes) {

  assertthat::assert_that(
    inherits(hcl_palettes, "hcl_palettes"),
    msg = "Argument `hcl_palettes` must inherit from colorspace `hcl_palettes`."
  )

  # qualitative, if h2 not set, use h1 + 360
}
