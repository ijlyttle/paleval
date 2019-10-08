#' Get HCL data for a series of hex-colors
#'
#' @inheritParams pev_data_separation
#' @inheritParams pev_hex_distance
#' @param ... other args (not used)
#'
#' @return `data.frame` with variables `cvd`, `x`, `hex`, `hue`, `chroma`, `luminance`
#'
pev_data_bloom  <- function(hex, include_cvd = TRUE, ...) {
  UseMethod("pev_data_bloom")
}

#' @rdname pev_data_bloom
#' @export
#'
pev_data_bloom.default <- function(hex, ...) {
  stop(
    glue::glue("No method for `pev_data_bloom` for class {class(hex)}"),
    call. = FALSE
  )
}

#' @rdname pev_data_bloom
#' @export
#'
pev_data_bloom.character <- function(hex, n = NULL, include_cvd = TRUE, ...) {

  # case: name of colorspace
  if (identical(length(hex), 1L) && !is_hexcolor(hex)) {
    fcont <- pev_fcont(hex)
    data_bloom <- pev_data_bloom.pev_fcont(fcont, n = n, include_cvd = include_cvd)

    return(data_bloom)
  }

  # case: error
  if (!all(is_hexcolor(hex))) {
    stop("cannot process character", call. = FALSE)
  }

  # case: hex-colors
  cvd <- get_cvd(include_cvd)

  data_bloom <-
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

  data_bloom$data <- purrr::map(data_bloom$hex, .hcl)

  data_bloom <- tidyr::unnest(data_bloom, cols = c("hex", "data"))

  data_bloom <- data_bloom[c("cvd", "x", "hex", "hue", "chroma", "luminance")]

  data_bloom
}

#' @rdname pev_data_bloom
#' @export
#'
pev_data_bloom.pev_fcont <- function(hex, n = NULL, include_cvd = TRUE, ...) {

  # rename for clarity
  fcont <- hex

  # call as unbounded discrete function
  fdisc <- pev_fdisc(fcont, method = "post")

  pev_data_bloom(fdisc, n = n, include_cvd = include_cvd)
}

#' @rdname pev_data_bloom
#' @export
#'
pev_data_bloom.pev_fdisc <- function(hex, n = NULL, include_cvd = TRUE, ...) {

  # rename for clarity
  fdisc <- hex

  n <- n %||% pev_nmax_display(fdisc)

  # get hex-colors from palette-function
  hex_new <- fdisc(n)

  # call as hex
  pev_data_bloom(hex_new, include_cvd = include_cvd)
}

.hcl <- function(hex) {

  rgb <- colorspace::hex2RGB(hex)
  hcl <- methods::as(rgb, "polarLUV")

  tibble::tibble(
    x = seq(0, 1, length.out = length(hex)),
    hue = hcl@coords[, "H"],
    chroma = hcl@coords[, "C"],
    luminance = hcl@coords[, "L"]
  )
}
