#' Coerce to continuous-palette function
#'
#' A continuous-palette function takes a vector of numbers, each between
#' 0 and 1, and returns a vector of character strings, each containing the
#' correpsonding hex-code. You con use a continuous palette-function to
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
#'   \item{`character`}{Name of a `palette` used by [colorspace::hcl_palettes].}
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
#'   \item{[pev_fcont_diverging()]}{Create a diverging-palette function from two functions.}
#'   \item{[pev_fcont_rescale()]}{Rescale input to continuous-palette function.}
#'   \item{[pev_fcont_reverse()]}{Reverse palette-function.}
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

#' @rdname pev_fcont
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

  assertthat::assert_that(
    assertthat::is.string(.fcont)
  )

  # TODO: trycatch to give better error message

  pev_hcl_list <- pev_map_hcl(colorspace::hcl_palettes(palette = .fcont))

  pev_hcl <- pev_hcl_list[[1]]

  f <- pev_fcont(pev_hcl)

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

  f <- function(i) {
    seqhcl(
      i = i,
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
    identical(.fcont(c(0, 0.5)), c(.fcont(0), .fcont(0.5)))
  )

  invisible(.fcont)
}
