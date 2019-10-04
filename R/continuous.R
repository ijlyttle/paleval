#' Modify output to simulate color-vision deficiency
#'
#' TODO: URL to explain CVD
#'
#' @param type `character`, describes color-vision deficiency. One of
#'   `"deutan"`, `"protan"`, `"tritan"`, `"none"`.
#' @param severity `numeric`, number between 0 (none) and 1 to describe
#'   the severity of color-vision deficiency.
#'
#' @inherit pev_fcont params return
#' @examples
#'   # Create diverging-palette function
#'   fcont_purple_green <- pev_fcont("Purple-Green")
#'   fcont_purple_green
#'
#'   # Simulate color-vision deficiency
#'   pev_fcont_cvd(fcont_purple_green, type = "deutan")
#'
#' @export
#'
pev_fcont_cvd <- function(.fcont,
                          type = c("deutan", "protan", "tritan", "none"),
                          severity = 1) {

  # validate input
  .fcont <- pev_fcont(.fcont)
  type <- match.arg(type)

  if (identical(type, "none")) {
    # no-op
    return(.fcont)
  }

  fcvd_list <- list(
    deutan = colorspace::deutan,
    protan = colorspace::protan,
    tritan = colorspace::tritan
  )

  fcvd <- fcvd_list[[type]]

  f <- function(x) {
    fcvd(.fcont(x))
  }

  pev_fcont(f)
}

#' Create continuous-palette function from HCL parameters
#'
#' @param .fcont_low `object` that can be coerced to `pev_fcont`,
#'   similar to `.fcont`, used for the low end of a diverging scale.
#'   Low end of this scale becomes the middle of the diverging scale.
#' @param .fcont_high `object` that can be coerced to `pev_fcont`,
#'   similar to `.fcont`, used for the high end of a diverging scale.
#'   Low end of this scale becomes the middle of the diverging scale.
#'
#' @inherit pev_fcont return
#'
#' @examples
#'   # Create sequential-palette functions
#'   fcont_purple <- pev_fcont("Purples 3")
#'   fcont_green <- pev_fcont("Greens 3")
#'
#'   # Create diverging-palette function
#'   pev_fcont_diverging(fcont_purple, fcont_green)
#' @export
#'
pev_fcont_diverging <- function(.fcont_low, .fcont_high) {

  # validate input
  pev_fcont_low <- pev_fcont(.fcont_low)
  pev_fcont_high <- pev_fcont(.fcont_high)

  f <- function(x) {
    x_rescale <- abs(x - 0.5) / 0.5

    ifelse(
      x < 0.5,
      pev_fcont_low(x_rescale),
      pev_fcont_high(x_rescale)
    )
  }

  pev_fcont(f)
}

#' Rescale input to continuous-palette function
#'
#' @param limits `numeric` vector of length 2, rescaling parameters.
#'   The input values to the returned function, `c(0, 1)`,
#'   are mapped to `limits` as inputs to the input function.
#'
#' @inherit pev_fcont return params
#'
#' @examples
#'   # Create sequential-palette function
#'   fcont_purple <- pev_fcont("Purples 3")
#'
#'   # Rescale palette-function
#'   pev_fcont_rescale(fcont_purple, limits = c(0.25, 0.75))
#'
#' @export
#'
pev_fcont_rescale <- function(.fcont, limits = c(0, 1)) {

  # validate input
  .fcont <- pev_fcont(.fcont)

  assertthat::assert_that(
    is.numeric(limits),
    identical(length(limits), 2L),
    min(limits) >= 0,
    max(limits) <= 1
  )

  f <- function(x) {

    x_rescale <- limits[1] + x * (limits[2] - limits[1])

    .fcont(x_rescale)
  }

  pev_fcont(f)
}

#' Reverse palette-function
#'
#' @inherit pev_fcont return params
#' @examples
#'   # Create sequential-palette function
#'   fcont_purple <- pev_fcont("Purples 3")
#'
#'   # Reverse palette-function
#'   pev_fcont_reverse(fcont_purple)
#'
#' @export
#'
pev_fcont_reverse <- function(.fcont) {

  # validate input
  .fcont <- pev_fcont(.fcont)

  pev_fcont_rescale(.fcont, limits = c(1, 0))
}

pev_rescale_diverging <- function(limit) {

  assertthat::assert_that(
    assertthat::is.scalar(limit),
    limit >= 0,
    limit <= 1
  )

  c(0.5 - limit / 2, 0.5 + limit / 2)
}

#' @export
#'
print.pev_fcont <- function(x, ...) {
  img_cont_ramp(x)
}
