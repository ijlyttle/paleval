#' Create HCL parameter-set
#'
#' This follows the colorspace scheme.
#'
#' @inheritParams colorspace::sequential_hcl
#'
#' @return Object with S3 class `pev_hcl`
#' @examples
#'  pev_hcl(h1 = 0, h2 = 360, c1 = 60, l1 = 60)
#' @export
#'
pev_hcl <- function(type = c("qualitative", "sequential", "diverging"),
                          h1, h2 = NULL, c1, c2 = NULL,
                          l1, l2 = NULL, p1 = NULL, p2 = NULL, cmax = NULL,
                          fixup = TRUE) {

  `%|na|%` <- function(x, y) {
    if (is.null(x) || is.na(x)) {
      return(y)
    }

    x
  }

  if (identical(type, "qualitative")) {
    h2 <- h2 %|na|% (h1 + 360)
    c2 <- c2 %|na|% c1
  } else {
    h2 <- h2 %|na|% h1
    c2 <- c2 %|na|% 0
  }

  l2 <- l2 %|na|% l1
  p1 <- p1 %|na|% 1
  p2 <- p2 %|na|% p1

  if (identical(type, "diverging")) {
    c2 <- c2 %|na|% NA_real_
  } else {
    c2 <- c2 %|na|% c1
    l2 <- l2 %|na|% l1
  }

  # want to permit NA here
  cmax <- cmax %||% NA_real_

  pev_hcl = structure(
    list(
      type = match.arg(type),
      h1 = h1,
      h2 = h2,
      c1 = c1,
      c2 = c2,
      l1 = l1,
      l2 = l2,
      p1 = p1,
      p2 = p2,
      cmax = cmax,
      fixup = fixup
    ),
    class = "pev_hcl"
  )

}

print.pev_hcl <- function(x, ...) {
  f <- function(x) {
    sprintf("% 7.2f", x)
  }

  print(
    glue::glue(
      "HCL Parameters (colorspace)",
      "Type:\t\t{x$type}",
      "Hue:\t\t[1]: {f(x$h1)}\t[2]: {f(x$h2)}",
      "Chroma:\t\t[1]: {f(x$c1)}\t[2]: {f(x$c2)}\tmax: {f(x$cmax)}",
      "Luminance:\t[1]: {f(x$l1)}\t[2]: {f(x$l2)}",
      "Exponent:\t[1]: {f(x$p1)}\t[2]: {f(x$p2)}",
      .sep = "\n"
    )
  )

  invisible(x)
}

#' Create list of HCL parameter-sets
#'
#' For each row in the `hcl_palettes` data frame
#'
#' @param hcl_palettes Object with S3 class `hcl_palettes`,
#'   created using [colorspace::hcl_palettes()].
#'
#' @return Named list, elements are objects with S3 class `pev_hcl`
#' @export
#'
pev_map_hcl <- function(hcl_palettes) {

  assertthat::assert_that(
    inherits(hcl_palettes, "hcl_palettes"),
    msg = "Argument `hcl_palettes` must inherit from colorspace `hcl_palettes`."
  )

  # fix type -> one of "qualitative", "sequential", "diverging"
  levels(hcl_palettes$type) <-
    c("qualitative", "sequential", "sequential", "diverging")

  hcl_palettes$type <- as.character(hcl_palettes$type)

  # want to convert to named list of lists: one element for each row
  hcl_list <-
    stats::setNames(
      split(hcl_palettes, seq(nrow(hcl_palettes))),
      rownames(hcl_palettes)
    )

  hcl_list <- purrr::map(hcl_list, as.list)

  hcl_list <- purrr::map(hcl_list, ~do.call(pev_hcl, .x))

  hcl_list
}
