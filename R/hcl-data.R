#' Get HCL data on series of hex-colors
#'
#' TODO: some notes here on `is_rgb_limit` being applied to
#' non-cvd case.
#'
#' @inheritParams pev_data_separation
#' @inheritParams pev_hex_distance
#' @param ... other args (not used)
#'
#' @return `data.frame` with variables `cvd`, `x`, `hex`,
#'   `hue`, `chroma`, `luminance`, `is_rgb_limit`
#'
#' @examples
#'   pev_data_hcl("Viridis")
#'   pev_data_hcl(grDevices::heat.colors(10))
#' @export
#'
pev_data_hcl  <- function(hex, ...) {
  UseMethod("pev_data_hcl")
}

#' @rdname pev_data_hcl
#' @export
#'
pev_data_hcl.default <- function(hex, ...) {
  stop(
    glue::glue("No method for `pev_data_hcl` for class {class(hex)}"),
    call. = FALSE
  )
}

#' @rdname pev_data_hcl
#' @export
#'
pev_data_hcl.character <- function(hex, n = NULL, include_cvd = TRUE, ...) {

  # case: name of colorspace
  if (identical(length(hex), 1L) && !is_hexcolor(hex)) {
    fcont <- pev_fcont(hex)
    data_hcl <- pev_data_hcl.pev_fcont(fcont, n = n, include_cvd = include_cvd)

    return(data_hcl)
  }

  hex <- as_hexcolor(hex)

  # case: hex-colors
  cvd <- get_cvd(include_cvd)

  data_hcl <-
    tibble::tibble(
      cvd = cvd,
      hex = purrr::map(
        cvd,
        function(cvd, hex) {
          .f <- .pev_cvd(cvd)
          .f(hex)
        },
        hex
      )
    )

  data_hcl$data <- purrr::map(data_hcl$hex, .hcl)

  data_hcl <- tidyr::unnest(data_hcl, cols = c("hex", "data"))

  data_hcl <-
    data_hcl[c("cvd", "x", "hex", "hue", "chroma", "luminance", "is_rgb_limit")]

  # amend is_rgb_limit to take it from non-cvd case
  is_rgb_limit_none <- data_hcl[data_hcl$cvd == "none", ]$is_rgb_limit
  data_hcl$is_rgb_limit <- rep(is_rgb_limit_none, times = length(cvd))

  data_hcl
}

#' @rdname pev_data_hcl
#' @export
#'
pev_data_hcl.pev_fcont <- function(hex, n = NULL, include_cvd = TRUE, ...) {

  # rename for clarity
  fcont <- hex

  # call as unbounded discrete function
  fdisc <- pev_fdisc(fcont, method = "post")

  pev_data_hcl(fdisc, n = n, include_cvd = include_cvd)
}

#' @rdname pev_data_hcl
#' @export
#'
pev_data_hcl.pev_fdisc <- function(hex, n = NULL, include_cvd = TRUE, ...) {

  # rename for clarity
  fdisc <- hex

  n <- n %||% pev_nmax_display(fdisc)

  # safety for bounded functions
  n <- min(n, pev_nmax(fdisc))

  # get hex-colors from palette-function
  hex_new <- fdisc(n)

  # call as hex
  pev_data_hcl(hex_new, include_cvd = include_cvd)
}

.hcl <- function(hex) {

  rgb <- colorspace::hex2RGB(hex)
  hcl <- methods::as(rgb, "polarLUV")
  is_rgb_limit <- is_rgb_limit(hex)

  tibble::tibble(
    x = seq(0, 1, length.out = length(hex)),
    hue = hcl@coords[, "H"],
    chroma = hcl@coords[, "C"],
    luminance = hcl@coords[, "L"],
    is_rgb_limit = is_rgb_limit
  )
}
