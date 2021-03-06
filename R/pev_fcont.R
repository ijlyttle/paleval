#' Coerce to continuous-palette function
#'
#' A continuous-palette function takes a vector of numbers, each between
#' 0 and 1, and returns a vector of character strings, each containing the
#' correpsonding hex-code. You con use a continuous-palette function to
#' build a custom ggplot2 scale, using [ggplot2::continuous_scale()].
#'
#' These functions help you build, modify, and compose continuous-palette
#' functions. By working with functions, rather than with a finite set of
#' hex-colors, we can avoid interpolation-errors as we compose and rescale.
#'
#' A continuous-palette function can be constructed using [pev_fcont()]; it takes
#' a single argument `.fcont`, which can be one of:
#'
#' \describe{
#'   \item{`character`}{(scalar) name of a `palette`, used by [colorspace::hcl_palettes], or
#'     (vector) hex-colors, used by [scales::colour_ramp()] }
#'   \item{`pev_hcl`}{Set of HCL parameters returned by [pev_hcl()].}
#'   \item{`pev_fcont`}{If you provide a `pev_fcont`, this is a no-op.}
#' }
#'
#' The print method for a `pev_fcont` function generates a plot of the palette.
#'
#' The other functions that return continuous-palette functions are:
#'
#' \describe{
#'   \item{[pev_fcont_cvd()]}{Modify output to simulate color-vision deficiency.}
#'   \item{[pev_fcont_rescale()]}{Rescale input to continuous-palette function.}
#'   \item{[pev_fcont_reverse()]}{Reverse palette-function.}
#'   \item{[pev_fcont_diverging()]}{Create a diverging-palette function from two functions.}
#' }
#'
#' @param .fcont `object` that can be coerced to `pev_fcont`,
#'   when called with a numeric vector with values
#'   between 0 and 1, returns the corresponding (hex-code) values.
#' @param ... other arguments (not used).
#'
#' @return `function` with S3 class `pev_fcont`,
#'  when called with a numeric vector with values between 0 and 1,
#'  returns the corresponding (hex-code) values.
#'
#' @examples
#' # Use a colorspace palette
#' pev_fcont("Blues 2")
#'
#' @export
#'
pev_fcont <- function(.fcont, ...) {
  UseMethod("pev_fcont")
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont.default <- function(.fcont, ...) {
  stop(
    glue::glue("No method for `pev_fcont` for class {class(.fcont)}"),
    call. = FALSE
  )
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont.pev_fcont <- function(.fcont, ...) {
  # no-op
  .fcont
}

#' @export
#'
pev_fcont.function <- function(.fcont, ...) {
  # validate & create
  f <- validate_pev_fcont(.fcont)
  f <- new_pev_cont(f)

  f
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont.character <- function(.fcont, ...) {

  # assume this is a colorspace-palette name
  if (identical(length(.fcont), 1L)) {

    # TODO: trycatch to give better error message
    pev_hcl_list <- pev_map_hcl(colorspace::hcl_palettes(palette = .fcont))
    pev_hcl <- pev_hcl_list[[1]]

    f <- pev_fcont(pev_hcl)

    return(f)
  }

  # assume this is a hex-color sequence
  assertthat::assert_that(
    all(is_hexcolor(.fcont))
  )

  f <- scales::colour_ramp(.fcont, alpha = FALSE)
  f <- validate_pev_fcont(f)
  f <- new_pev_cont(f)

  f
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont.pev_hcl <- function(.fcont, ...) {

  # if type is diverging, construct two sequential scales and compose
  if (identical(.fcont$type, "diverging")) {

    hcl_low <- pev_hcl(
      type = "sequential",
      h1 = .fcont$h1,
      c1 = .fcont$c1,
      c2 = 0,
      l1 = .fcont$l1,
      l2 = .fcont$l2,
      p1 = .fcont$p1,
      p2 = .fcont$p2,
      cmax = .fcont$cmax,
      fixup = .fcont$fixup
    )

    hcl_high <- pev_hcl(
      type = "sequential",
      h1 = .fcont$h2,
      c1 = .fcont$c1,
      c2 = 0,
      l1 = .fcont$l1,
      l2 = .fcont$l2,
      p1 = .fcont$p1,
      p2 = .fcont$p2,
      cmax = .fcont$cmax,
      fixup = .fcont$fixup
    )

    f <- pev_fcont_diverging(pev_fcont(hcl_low), pev_fcont(hcl_high))
    f <- validate_pev_fcont(f)
    f <- new_pev_cont(f)

    return(f)
  }

  f <- function(x) {
    seqhcl(
      i = x,
      h1 = .fcont$h1,
      h2 = .fcont$h2,
      c1 = .fcont$c1,
      c2 = .fcont$c2,
      l1 = .fcont$l1,
      l2 = .fcont$l2,
      p1 = .fcont$p1,
      p2 = .fcont$p2,
      cmax = .fcont$cmax,
      fixup = .fcont$fixup
    )
  }

  f <- validate_pev_fcont(f)
  f <- new_pev_cont(f)

  f
}

# internal function just to make sure we have the right classes
new_pev_cont <- function(.fcont) {

  class(.fcont) <- unique(c("pev_fcont", class(.fcont)))

  .fcont
}

# internal function to validate that a pev_fcont does what it needs to
validate_pev_fcont <- function(.fcont) {

  # verify that the function does what it claims
  assertthat::assert_that(
    inherits(.fcont, "function"),
    is_hexcolor(.fcont(1)),
    identical(.fcont(c(0, 0.5)), c(.fcont(0), .fcont(0.5)))
  )

  invisible(.fcont)
}

#' @export
#'
print.pev_fcont <- function(x, ...) {
  img_cont_ramp(x)
}
