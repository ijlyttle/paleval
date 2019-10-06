#' Calculate distance between two (sets of) colors
#'
#' This returns the distance, according to the `method`,
#' between corresponding hex-colors in `hex` and `hex_ref`.
#'
#' The vectors `hex` and `hex_ref` must be the same length.
#'
#' @param hex `character` vector of hex-colors
#' @param hex_ref `character` vector of hex-colors
#' @param method `character` method to use for distance calculation,
#'   passed to `farver::compare_color()`.
#'   One of: `"euclidean"`, `"cie1976"`, `"cie94"`, `"cie2000"`, or `"cmc"`.
#'
#' @return `numerical` vector, same length as `hex` and `hex_ref`.
#' @examples
#'   pev_hex_distance("#000000", "#FFFFFF")
#'   pev_hex_distance(c("#000000", "#FFFFFF"), c("#000000", "#000000"))
#'   pev_hex_distance(c("#000000", "#FFFFFF"), "#000000")
#' @export
#'
pev_hex_distance <- function(hex, hex_ref, method = "cie2000") {

  assertthat::assert_that(
    is_hexcolor(hex),
    is_hexcolor(hex_ref),
    method %in% c("euclidean", "cie1976", "cie94", "cie2000", "cmc")
  )

  # recycle
  if (identical(length(hex), 1L)) {
    hex <- rep(hex, length(hex_ref))
  }

  if (identical(length(hex_ref), 1L)) {
    hex_ref <- rep(hex_ref, length(hex))
  }

  if (!identical(length(hex), length(hex_ref))) {
    stop("Cannot reconcile length of `hex` and `hex_ref`", call. = FALSE)
  }

  list_rgb <- function(x) {
    purrr::map(x, ~t(grDevices::col2rgb(.x)))
  }

  rgb <- list_rgb(hex)
  rgb_ref <- list_rgb(hex_ref)

  distance <-
    purrr::map2_dbl(
      rgb,
      rgb_ref,
      farver::compare_colour,
      from_space = "rgb",
      method = method
    )

  distance
}
