#' Coerce to discrete-palette function
#'
#' A discrete-palette function takes an integers, from 1 to
#' the maximum size of the palette, and returns a vector of character hex-codes.
#' You con use a discrete-palette function to
#' build a custom ggplot2 scale, using [ggplot2::discrete_scale()].
#'
#' These functions help you build, modify, and compose discrete-palette
#' functions.
#'
#' A discrete-palette function can be *bounded* or *unbounded*. A bounded
#' function has a maxiumum number of possible colors, and is created from a
#' series of hex-colors, for example,
#' [Tableau 10](https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782).
#' An unbounded function is created from a continuous-palette function, thus has no maximum
#' number of possible colors.
#'
#' A discrete-palette function can be constructed using [pev_fdisc()]; it takes
#' an argument `.fdisc`, which can be one of:
#'
#' \describe{
#'   \item{`character`}{A series of hex-codes,
#'     returns a **bounded** discrete-palette function.}
#'   \item{`pev_fcont`}{A continuous-palette function, to be discretized
#'     according to `method` (`"post"` or `"panel"`).
#'     This returns an **unbounded** discrete-palette function.}
#'   \item{`pev_fdisc`}{If you provide a `pev_fdisc`, this is a no-op.}
#' }
#'
#' The print method for a `pev_disc` function generates a plot of the palette.
#'
#' The other functions that return continuous-palette functions are:
#'
#' \describe{
#'   \item{[pev_fdisc_cvd()]}{Modify output to simulate color-vision deficiency.}
#'   \item{[pev_fdisc_reverse()]}{Reverse palette-function.}
#' }
#'
#' @param .fdisc `object` that can be coerced to `pev_fdisc`,
#'   when called with an integer, returns the corresponding (hex-code) values.
#' @param method `character`, describes how the domain of the continuous palette
#'   is to be discretized, can be `"post"` or `"panel"` (using a fencing analogy).
#' @param ... other arguments (not used).
#'
#' @return `function` with S3 class `pev_disc`,
#'  when called with a numeric vector with values between 0 and 1,
#'  returns the corresponding (hex-code) values.
#'
#' @examples
#'  # Use a colorspace palette
#'  pev_fdisc(pev_fcont("Blues 2"), n = 11, method = "post")
#'
#' @export
#'
pev_fdisc <- function(.fdisc, ...) {
  UseMethod("pev_fdisc")
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.default <- function(.fdisc, ...) {
  stop(
    glue::glue("No method for `pev_fdisc` for class {class(.fdisc)}"),
    call. = FALSE
  )
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.character <- function(.fdisc, ...) {

  assertthat::assert_that(
    all(is_hexcolor(.fdisc))
  )

  f <- function(i) {
    .fdisc[seq_len(i)]
  }

  attr(f, "n_max") <- length(.fdisc)
  f <- validate_pev_fbounded(f)
  f <- new_pev_fbounded(f)

  f
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.pev_fcont <- function(.fdisc, method = c("post", "panel"), ...) {

  method <- match.arg(method)

  f <- function(n) {
    # determine interpolation points
    if (identical(method, "post")) {
      values <- seq(0, n - 1) / as.double(n - 1)
    } else {
      values <- (seq(0, n - 1) + 0.5) / as.double(n)
    }

    # interpolate
    hex_colors <- .fdisc(values)

    hex_colors
  }

  f <- validate_pev_funbounded(f)
  f <- new_pev_funbounded(f)

  f
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.pev_fdisc <- function(.fdisc, ...) {
  # no-op
  .fdisc
}

pev_nmax <- function(.fdisc) {

  .fdisc <- pev_fdisc(.fdisc)

  if (inherits(.fdisc, "pev_funbounded")) {
    n_max <- Inf
  } else {
    n_max <- as.double(attr(.fdisc, "n_max"))
  }

  n_max
}

pev_nmax_display <- function(.fdisc) {

  if (inherits(.fdisc, "pev_funbounded")) {
    n <- 11
  }

  if (inherits(.fdisc, "pev_fbounded")) {
    n <- pev_nmax(.fdisc)
  }

  n
}

# internal function
new_pev_fbounded <- function(.fdisc) {

  class(.fdisc) <-
    unique(c("pev_fbounded", "pev_fdisc", class(.fdisc)))

  .fdisc
}

# internal function
validate_pev_fbounded <- function(.fdisc) {

  n_max <- attr(.fdisc, "n_max")
  # verify that the function does what it claims
  assertthat::assert_that(
    inherits(.fdisc, "function"),
    is_hexcolor(.fdisc(n_max))
  )

  invisible(.fdisc)
}

# internal function
new_pev_funbounded <- function(.fdisc) {

  class(.fdisc) <-
    unique(c("pev_funbounded", "pev_fdisc", class(.fdisc)))

  .fdisc
}

# internal function
validate_pev_funbounded <- function(.fdisc) {

  # verify that the function does what it claims
  assertthat::assert_that(
    inherits(.fdisc, "function"),
    is_hexcolor(.fdisc(100))
  )

  invisible(.fdisc)
}

#' @export
#'
print.pev_fdisc <- function(x, ...) {
  img_disc(x)
}
