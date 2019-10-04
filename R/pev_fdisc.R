#' Coerce to discrete-palette function
#'
#' A discrete-palette function takes a vector of integers, from 1 to
#' the size of the palette, and returns a vector of character strings,
#' each containing the correpsonding hex-code.
#' You con use a discrete-palette function to
#' build a custom ggplot2 scale, using [ggplot2::discrete_scale()].
#'
#' These functions help you build, modify, and compose discrete-palette
#' functions.
#'
#' A discrete-palette function can be constructed using [pev_fdisc()]; it takes
#' an argument `.fdisc`, which can be one of:
#'
#' \describe{
#'   \item{`character`}{A series of hex-codes.}
#'   \item{`pev_fcont`}{This will discretize a continuous palette into `n` colors,
#'   according to the discretization `method` (`"post"` or `"panel"`).}
#'   \item{`pev_fdisc`}{If you provide a `pev_fdisc`, this is a no-op.}
#' }
#'
#' The print method for a `pev_disc` function generates a plot of the palette.
#'
#' The other functions that return continuous-palette functions are:
#'
#' \describe{
#'   \item{`pev_fpal_cvd()`}{Modify output to simulate color-vision deficiency.}
#'   \item{`pev_fpal_reverse()`}{Reverse palette-function.}
#' }
#'
#' @param .fdisc `object` that can be coerced to `pev_fdisc`,
#'   when called with an integer, returns the corresponding (hex-code) values.
#' @param n coercible to `integer`, number colors to put in the palette.
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

#' @export
#'
pev_fdisc.function <- function(.fdisc, ...) {
  # validate & create
  f <- validate_pev_fdisc(.fdisc)
  f <- new_pev_disc(f)

  f
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.character <- function(.fdisc, ...) {

  assertthat::assert_that(
    all(is_hexcolor(.fdisc))
  )

  f <- function(i = NULL) {
    if (is.null(i)) {
      return(.fdisc)
    }

    i <- as.integer(i)

    if (any(i > length(.fdisc))) {
      stop("Subscript out of range")
    }

    .fdisc[i]
  }

  f <- validate_pev_fcont(f)

  new_pev_disc(f)
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.pev_fcont <- function(.fdisc, n = 11, method = c("post", "panel"), ...) {

  method = match.arg(method)

  assertthat::assert_that(
    assertthat::is.number(n),
    assertthat::is.scalar(n)
  )

  n <- as.integer(n)

  # determine interpolation points
  if (identical(method, "post")) {
    values <- seq(0, n - 1) / as.double(n - 1)
  } else {
    values <- (seq(0, n - 1) + 0.5) / as.double(n)
  }

  # interpolate
  hex_colors <- .fdisc(values)

  pev_fdisc(hex_colors)
}

#' @rdname pev_fdisc
#' @export
#'
pev_fdisc.pev_fdisc <- function(.fdisc, ...) {
  # no-op
  .fdisc
}

# internal function just to make sure we have the right classes
new_pev_disc <- function(.fdisc) {

  class(.fdisc) <- unique(c("pev_fdisc", class(.fdisc)))

  .fdisc
}

# internal function to validate that a pev_fcont does what it needs to
validate_pev_fdisc <- function(.fdisc) {

  # verify that the function does what it claims
  assertthat::assert_that(
    inherits(.fdisc, "function"),
    is_hexcolor(.fdisc(1)),
    identical(.fdisc(c(1, 1)), c(.fdisc(1), .fdisc(1)))
  )

  invisible(.fdisc)
}

#' @export
#'
print.pev_fdisc <- function(x, ...) {
  img_disc(x)
}
