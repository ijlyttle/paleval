#' Create continuous-palette function
#'
#' @param hcl_param Object with S3 class `pev_hcl_param`,
#'   created using [pev_hcl_param()].
#' @param pev_fcont `function`, when called with a numeric vector with values
#'   between 0 and 1, returns the corresponding (hex-code) values.
#' @param pev_fcont_low `function`, similar to `pev_fcont`, used for the
#'   low end of a diverging scale. Low end of this scale becomes the
#'   middle of the diverging scale.
#' @param pev_fcont_high `function`, similar to `pev_fcont`, used for the
#'   high end of a diverging scale. Low end of this scale becomes the
#'   middle of the diverging scale.
#' @param limits `numeric` vector of length 2, rescaling parameters. The input
#'   values to the returned function, `c(0, 1)`, are mapped to `limits` as
#'   inputs to the input function.
#' @param type `character`, describes color-vision deficiency. One of
#'   `"deutan"`, `"protan"`, `"tritan"`, `"none"`.
#' @param severity `numeric`, number between 0 (none) and 1 to describe
#'   the severity of color-vision deficiency.
#'
#' @return `function`, when called with a numeric vector with values
#'  between 0 and 1, returns the corresponding (hex-code) values.
#' @examples
#'   # Define HCL parameters for palette functions
#'   pals <- pev_map_hcl_param(
#'     colorspace::hcl_palettes(palette = c("Purples 3", "Greens 3"))
#'   )
#'
#'   # Create palette functions
#'   fcont_purple <- pev_fcont(pals[["Purples 3"]])
#'   fcont_green <- pev_fcont(pals[["Greens 3"]])
#'   fcont_purple
#'   fcont_green
#'
#'   # Modify palette functions
#'   pev_fcont_reverse(fcont_purple)
#'   pev_fcont_rescale(fcont_purple, limits = c(0.25, 0.75))
#'
#'   # Create diverging-palette function
#'   fcont_purple_green <- pev_fcont_div(fcont_purple, fcont_green)
#'   fcont_purple_green
#'
#'   # Modify palette function to simulate color-vision deficiency
#'   pev_fcont_cvd(fcont_purple_green, type = "deutan")
#' @export
#'
pev_fcont <- function(hcl_param) {

  # if type is diverging, construct two sequential scales and compose
  if (identical(hcl_param$type, "diverging")) {

    hcl_low <- pev_hcl_param(
      type = "sequential",
      h1 = hcl_param$h1,
      c1 = hcl_param$c1,
      c2 = 0,
      l1 = hcl_param$l1,
      l2 = hcl_param$l2,
      p1 = hcl_param$p1,
      p2 = hcl_param$p2,
      cmax = hcl_param$cmax,
      fixup = hcl_param$fixup
    )

    hcl_high <- pev_hcl_param(
      type = "sequential",
      h1 = hcl_param$h2,
      c1 = hcl_param$c1,
      c2 = 0,
      l1 = hcl_param$l1,
      l2 = hcl_param$l2,
      p1 = hcl_param$p1,
      p2 = hcl_param$p2,
      cmax = hcl_param$cmax,
      fixup = hcl_param$fixup
    )

    f <- pev_fcont_div(pev_fcont(hcl_low), pev_fcont(hcl_high))

    return(f)
  }

  f <- function(x) {
    seqhcl(
      i = x,
      h1 = hcl_param$h1,
      h2 = hcl_param$h2,
      c1 = hcl_param$c1,
      c2 = hcl_param$c2,
      l1 = hcl_param$l1,
      l2 = hcl_param$l2,
      p1 = hcl_param$p1,
      p2 = hcl_param$p2,
      cmax = hcl_param$cmax,
      fixup = hcl_param$fixup
    )
  }

  .as_pev_fcont(f)
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont_div <- function(pev_fcont_low, pev_fcont_high) {

  f <- function(x) {
    x_rescale <- abs(x - 0.5) / 0.5

    ifelse(
      x < 0.5,
      pev_fcont_low(x_rescale),
      pev_fcont_high(x_rescale)
    )
  }

  .as_pev_fcont(f)
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont_rescale <- function(pev_fcont, limits = c(0, 1)) {

  assertthat::assert_that(
    is.numeric(limits),
    identical(length(limits), 2L),
    min(limits) >= 0,
    max(limits) <= 1
  )

  f <- function(x) {

    x_rescale <- limits[1] + x * (limits[2] - limits[1])

    pev_fcont(x_rescale)
  }

  .as_pev_fcont(f)
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont_reverse <- function(pev_fcont) {
  pev_fcont_rescale(pev_fcont, limits = c(1, 0))
}

#' @rdname pev_fcont
#' @export
#'
pev_fcont_cvd <- function(pev_fcont,
                          type = c("deutan", "protan", "tritan", "none"),
                          severity = 1) {

  type <- match.arg(type)

  if (identical(type, "none")) {
    # no-op
    return(pev_fcont)
  }

  fcvd_list <- list(
    deutan = colorspace::deutan,
    protan = colorspace::protan,
    tritan = colorspace::tritan
  )

  fcvd <- fcvd_list[[type]]

  f <- function(x) {
    fcvd(pev_fcont(x))
  }

  .as_pev_fcont(f)
}

# internal function to add `pev_fcont` class
.as_pev_fcont <- function(x) {
  class(x) <- unique(c("pev_fcont", class(x)))

  x
}

pev_rescale_div <- function(limit) {

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
