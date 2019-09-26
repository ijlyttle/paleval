#' Coerce object to vector of hex-codes
#'
#' This function coerces to a character vector, then validates that each
#' element of the vector is a valid hex-code.
#'
#' @param x `object` to be coerced
#'
#' @return `character` vector of hex-codes
#' @noRd
#'
as_hexcolor <- function(x) {

  # coerce to character vector
  x <- unlist(x)
  x <- unname(x)
  x <- as.character(x)

  assertthat::assert_that(
    is_hexcolor(x),
    msg = "object not coercible to hexcolor"
  )

  x
}

is_hexcolor <- function(x) {

  # ^#              string starts with `#`
  # [A-Fa-f0-9]{6}  exactly six of the characters in `A-F`, `a-f`, `0-9`
  # $               end of string
  regex <- "^#[A-Fa-f0-9]{6}$"

  all(grepl(regex, x))
}
