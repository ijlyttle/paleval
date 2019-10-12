#' Get minimum perceptual-distance from reference color
#'
#' An approximation, for now.
#'
#' @inheritParams pev_hex_distance
#' @inheritParams pev_data_hcl
#' @param .fpal palette-function.
#' @param n `integer`, number of points to discretize the range (0, 1)
#'   of the palette-function to determine the minimum perceptual-distance.
#'
#' @return `data.frame` with variables `cvd`, `x_nearest`, `distance_nearest`,
#'  `hex`, `hex_ref`, `hue`, `chroma`, `luminance`.
#' @examples
#'   pev_data_hcl_ref("Viridis", "#008F97")
#' @export
#'
pev_data_hcl_ref <- function(.fpal, hex_ref, n = NULL,
                             method = "cie2000", include_cvd = TRUE,
                             ...) {
    UseMethod("pev_data_hcl_ref")
}

#' @rdname pev_data_hcl_ref
#' @export
#'
pev_data_hcl_ref.default <- function(.fpal, hex_ref, n = NULL,
                                     method = "cie2000", include_cvd = TRUE,
                                     ...) {
  stop(
    glue::glue("No method for `pev_data_hcl_ref` for class {class(.fpal)}"),
    call. = FALSE
  )
}

#' @rdname pev_data_hcl_ref
#' @export
#'
pev_data_hcl_ref.character <-  function(.fpal, hex_ref, n = NULL,
                                        method = "cie2000", include_cvd = TRUE,
                                        ...) {

  # name of a colorspace palette -> continuous palette-function
  .fcont <- pev_fcont(.fpal)

  pev_data_hcl_ref(
    .fcont,
    hex_ref = hex_ref,
    n = n,
    method = method,
    include_cvd = include_cvd,
    ...
  )
}

#' @rdname pev_data_hcl_ref
#' @export
#'
pev_data_hcl_ref.pev_fcont <-  function(.fpal, hex_ref, n = NULL,
                                         method = "cie2000", include_cvd = TRUE,
                                         ...) {

  n <- n %||% 151

  # discretize
  x <- seq(0, n - 1) / (n - 1)
  hex <- .fpal(x)

  .hex_hcl_ref(hex, hex_ref, method = method, include_cvd = include_cvd)
}

#' @rdname pev_data_hcl_ref
#' @export
#'
pev_data_hcl_ref.pev_funbounded <-  function(.fpal, hex_ref, n = NULL,
                                             method = "cie2000",
                                             include_cvd = TRUE,
                                             ...) {

  n <- n %||% 151

  # discretize
  hex <- .fpal(n)

  .hex_hcl_ref(hex, hex_ref, method = method, include_cvd = include_cvd)
}


#' @rdname pev_data_hcl_ref
#' @export
#'
pev_data_hcl_ref.pev_fbounded <-  function(.fpal, hex_ref, n = NULL,
                                          method = "cie2000",
                                          include_cvd = TRUE,
                                          ...) {
  n <- n %||% pev_nmax(.fpal)
  n <- min(n, pev_nmax(.fpal))

  # discretize
  hex <- .fpal(n)

  .hex_hcl_ref(hex, hex_ref, method = method, include_cvd = include_cvd)
}

# return `data.frame` with one row for each combination of hex_ref & cvd
#   `cvd`,` x_nearest`, `distance_nearest`, `hex_nearest`
#   `hex_ref`, `hue_ref`, `chroma_ref`, `luminance_ref`
#
.hex_hcl_ref <- function(hex, hex_ref, method = "cie2000", include_cvd = TRUE) {
  purrr::map_df(
    hex_ref,
    .hex_hcl_ref_single,
    hex = hex,
    method = method,
    include_cvd = include_cvd
  )
}

# return `data.frame` with rows for one hex_ref
#   `cvd`,` x_nearest`, `distance_nearest`, `hex_nearest`
#   `hex_ref`, `hue_ref`, `chroma_ref`, `luminance_ref`
#
.hex_hcl_ref_single <- function(hex_ref, hex, method = "cie2000",
                                include_cvd = TRUE) {

  # context: no cvd
  distance <- pev_hex_distance(hex, hex_ref, method = method)
  distance_nearest <- min(distance)

  # no cvd
  i_nearest <- which(distance == distance_nearest)[[1]]
  hex_nearest <- hex[[i_nearest]]
  x_nearest <- (i_nearest - 1) / (length(hex) - 1)

  # TODO: consider using Newton iteration....

  cvd <- get_cvd(include_cvd)

  hex_nearest_cvd <-
    purrr::map_chr(
      purrr::map(cvd, .pev_cvd),
      purrr::exec,
      hex_nearest
    )

  # `cvd` ,`hex_ref`, `hue_ref`, `chroma_ref`, `luminance_ref`
  .data <- pev_data_hcl(hex_ref, include_cvd = include_cvd)
  names(.data) <-
    c("cvd", "foo", "hex_ref",  "hue_ref", "chroma_ref", "luminance_ref", "foo")

  .data$x_nearest <- rep(x_nearest, length(cvd))
  .data$distance_nearest <- rep(distance_nearest, length(cvd))
  .data$hex_nearest <- hex_nearest_cvd

  result <- .data[,
    c("cvd",
      "x_nearest",
      "distance_nearest",
      "hex_nearest",
      "hex_ref",
      "hue_ref",
      "chroma_ref",
      "luminance_ref"
    )
  ]

  result
}
