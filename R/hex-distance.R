#' Calculate distance between two (sets of) colors
#'
#' This returns the distance, according to the `method`,
#' between corresponding hex-colors in `hex_a` and `hex_b`.
#'
#' The vectors `hex_a` and `hex_b` must be the same length.
#'
#' @param hex_a `character` vector of hex-colors
#' @param hex_b `character` vector of hex-colors
#' @param method `character` method to use for distance calculation,
#'   passed to `farver::compare_color()`.
#'   One of: `"euclidean"`, `"cie1976"`, `"cie94"`, `"cie2000"`, or `"cmc"`.
#'
#' @return `numerical` vector, same length as `hex_a` and `hex_b`.
#' @examples
#'   pev_hex_distance("#000000", "#FFFFFF")
#'   pev_hex_distance(c("#000000", "#FFFFFF"), c("#000000", "#000000"))
#'   pev_hex_distance(c("#000000", "#FFFFFF"), "#000000")
#' @export
#'
pev_hex_distance <- function(hex_a, hex_b, method = "cie2000") {

  assertthat::assert_that(
    is_hexcolor(hex_a),
    is_hexcolor(hex_b),
    method %in% c("euclidean", "cie1976", "cie94", "cie2000", "cmc")
  )

  # recycle
  if (identical(length(hex_a), 1L)) {
    hex_a <- rep(hex_a, length(hex_b))
  }

  if (identical(length(hex_b), 1L)) {
    hex_b <- rep(hex_b, length(hex_a))
  }

  if (!identical(length(hex_a), length(hex_b))) {
    stop("Cannot reconcile length of `hex_a` and `hex_b`", call. = FALSE)
  }

  list_rgb <- function(x) {
    purrr::map(x, ~t(grDevices::col2rgb(.x)))
  }

  rgb_a <- list_rgb(hex_a)
  rgb_b <- list_rgb(hex_b)

  distance <-
    purrr::map2_dbl(
      rgb_a,
      rgb_b,
      farver::compare_colour,
      from_space = "rgb",
      method = method
    )

  distance
}
