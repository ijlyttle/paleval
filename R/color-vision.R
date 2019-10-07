#' Modify output to simulate color-vision deficiency
#'
#' TODO: URL to explain CVD
#'
#' @inheritParams pev_fcont
#' @inheritParams pev_fdisc
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
#'   pev_fcont_cvd(fcont_purple_green, type = "deutan")
#'   pev_fdisc_cvd(fdisc_purple_green, type = "deutan")
#'
#' @export
#'
pev_fcont_cvd <- function(.fcont, ...) {
  UseMethod("pev_fcont_cvd")
}

#' @rdname pev_fcont_cvd
#' @export
#'
pev_fcont_cvd.default <- function(.fcont, ...) {
  stop(
    glue::glue("No method for `pev_fcont_cvd` for class {class(.fcont)}"),
    call. = FALSE
  )
}

#' @rdname pev_fcont_cvd
#' @export
#'
pev_fcont_cvd.pev_fcont <- function(.fcont,
                                     type = c("deutan", "protan", "tritan", "none"),
                                     severity = 1, ...) {

  fcvd <- .pev_cvd(type = type, severity = severity)

  f <- function(x) {
    fcvd(.fcont(x))
  }

  new_pev_cont(f)
}


#' @rdname pev_fcont_cvd
#' @export
#'
pev_fdisc_cvd <- function(.fdisc, ...) {
  UseMethod("pev_fdisc_cvd")
}

#' @rdname pev_fcont_cvd
#' @export
#'
pev_fdisc_cvd.default <- function(.fdisc, ...) {
  stop(
    glue::glue("No method for `pev_fdisc_cvd` for class {class(.fdisc)}"),
    call. = FALSE
  )
}

#' @rdname pev_fcont_cvd
#' @export
#'
pev_fdisc_cvd.pev_funbounded <- function(.fdisc,
                                         type = c("deutan", "protan", "tritan", "none"),
                                         severity = 1, ...) {

  fcvd <- .pev_cvd(type = type, severity = severity)

  f <- function(n) {
    fcvd(.fdisc(n))
  }

  new_pev_funbounded(f)
}

#' @rdname pev_fcont_cvd
#' @export
#'
pev_fdisc_cvd.pev_fbounded <- function(.fdisc,
                                       type = c("deutan", "protan", "tritan", "none"),
                                       severity = 1, ...) {

  fcvd <- .pev_cvd(type = type, severity = severity)

  f <- function(n) {
    fcvd(.fdisc(n))
  }

  attr(f, "n_max") <- attr(.fdisc, "n_max")

  new_pev_fbounded(f)
}

# internal function, returns generic function
.pev_cvd <- function(type = c("deutan", "protan", "tritan", "none"),
                     severity = 1) {

    # validate input
    type <- match.arg(type)

    if (identical(type, "none")) {
      # no-op
      return(identity)
    }

    fcvd_list <- list(
      deutan = colorspace::deutan,
      protan = colorspace::protan,
      tritan = colorspace::tritan
    )

    fcvd <- fcvd_list[[type]]

    fcvd
  }

