#' Coerce object to vector of hex-codes
#'
#' This function coerces to a character vector, then validates that each
#' element of the vector is a valid hex-code.
#'
#' @param x `object` to be coerced
#'
#' @return `character` vector of hex-colors
#' @noRd
#'
as_hexcolor <- function(x) {

  # coerce to character vector
  x <- unlist(x)
  x <- unname(x)
  x <- as.character(x)

  x <- remove_alpha(x)

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

remove_alpha <- function(x) {
  # some hex-colors have an alpha channel, this removes that
  sub("^(#[A-Fa-f0-9]{6})[A-Fa-f0-9]{2}$", "\\1", x)
}

# determines if hexcolor is on an RGB boundary
is_rgb_limit <- function(x) {

  # works on a single RGB vector
  is_b <- function(.rgb) {

    rgb_num <- .rgb@coords[1, ]

    any(rgb_num == 0) | any(rgb_num == 1)
  }

  rgb <- purrr::map(x, colorspace::hex2RGB)

  result <- purrr::map_lgl(rgb, is_b)

  result
}
