#' Get perceptual-differences within palette
#'
#' @param pal `character` (coerceable to vector of hex-codes), describes a
#'   discrete color palette. Interpolation within a palette is not required to
#'   be a well-formed idea.
#'
#' @return `tbl_df` with variables `color_A`, `color_B`, `difference`
#' @export
#'
pev_data_separation <- function(pal) {

  # coerce to hexcolor
  pal <- as_hexcolor(pal)

  tibble::tibble()
}

