#' Calculate perceptual-distance between two (sets of) colors
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


#' Calculate perceptual-derivative for sequence of hex-colors
#'
#' This assumes that `hex` reperesents colors on a continuous-scale
#' where the domain varies uniformly from 0 to 1.
#'
#' @inheritParams pev_hex_distance
#'
#' @return `numeric`
#' @examples
#'   pev_fcont("Viridis")(seq(0, 1, by = 0.025)) %>%
#'     pev_hex_derivative()
#' @export
#'
pev_hex_derivative <- function(hex, method = "cie2000") {

  # validate arguments

  n <- length(hex)

  d_distance <- numeric(n)
  d_x <- 1 / (n - 1)

  i <- seq(2, n -1)

  dist <- function(hex, hex_ref) {
    pev_hex_distance(hex, hex_ref, method = method)
  }

  d_distance[1] <- 4 * dist(hex[2], hex[1]) - dist(hex[3], hex[1])
  d_distance[i] <- dist(hex[i + 1], hex[i - 1])
  d_distance[n] <- 4 * dist(hex[n], hex[n - 1]) - dist(hex[n], hex[n - 2])

  d_distance_d_x <- d_distance / (2 * d_x)

  d_distance_d_x
}
