#' Modify output to simulate color-vision deficiency
#'
#' TODO: URL to explain CVD
#'
#' @param .fpal Object with S3 class `pev_fcont` or `pev_fdisc`,
#'   a continuous or discrete palette-function
#' @param type `character`, describes color-vision deficiency. One of
#'   `"deutan"`, `"protan"`, `"tritan"`, `"none"`.
#' @param severity `numeric`, number between 0 (none) and 1 to describe
#'   the severity of color-vision deficiency.
#' @param ... Other args (not used)
#'
#' @return Object of of the same type as `.fpal`;
#'   a palette function, with S3 class `pev_fcont` or `pev_fdisc`.
#'
#' @examples
#'   # Create continuous diverging-palette function
#'   fcont_purple_green <- pev_fcont("Purple-Green")
#'   fcont_purple_green
#'
#'   # Create discrete diverging-palette function
#'   fdisc_purple_green <- pev_fdisc(fcont_purple_green, n = 11)
#'   fdisc_purple_green
#'
#'   # Simulate color-vision deficiency
#'   pev_fpal_cvd(fcont_purple_green, type = "deutan")
#'   pev_fpal_cvd(fdisc_purple_green, type = "deutan")
#'
#' @export
#'
pev_fpal_cvd <- function(.fpal, ...) {
  UseMethod("pev_fpal_cvd")
}

#' @rdname pev_fpal_cvd
#' @export
#'
pev_fpal_cvd.default <- function(.fpal, ...) {
  stop(
    glue::glue("No method for `pev_fpal_cvd` for class {class(.fpal)}"),
    call. = FALSE
  )
}

#' @rdname pev_fpal_cvd
#' @export
#'
pev_fpal_cvd.pev_fcont <- function(.fpal,
                                   type = c("deutan", "protan", "tritan", "none"),
                                   severity = 1, ...) {

  f <- .pev_fpal_cvd(.fpal, type = type, severity = severity)

  new_pev_cont(f)
}

#' @rdname pev_fpal_cvd
#' @export
#'
pev_fpal_cvd.pev_fcont <- function(.fpal,
                                   type = c("deutan", "protan", "tritan", "none"),
                                   severity = 1, ...) {

  f <- .pev_fpal_cvd(.fpal, type = type, severity = severity)

  new_pev_cont(f)
}

#' @rdname pev_fpal_cvd
#' @export
#'
pev_fpal_cvd.pev_fdisc <- function(.fpal,
                                   type = c("deutan", "protan", "tritan", "none"),
                                   severity = 1, ...) {

  f <- .pev_fpal_cvd(.fpal, type = type, severity = severity)

  new_pev_disc(f)
}

# internal function, returns generic function
.pev_fpal_cvd <- function(.fpal,
                          type = c("deutan", "protan", "tritan", "none"),
                          severity = 1) {

    # validate input
    type <- match.arg(type)

    if (identical(type, "none")) {
      # no-op
      return(.fpal)
    }

    fcvd_list <- list(
      deutan = colorspace::deutan,
      protan = colorspace::protan,
      tritan = colorspace::tritan
    )

    fcvd <- fcvd_list[[type]]

    f <- function(x = NULL) {
      fcvd(.fpal(x))
    }

    f
  }

