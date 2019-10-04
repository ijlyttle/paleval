fcont <- pev_fcont("Blues 2")

test_that("pev_fcont throws error for unknown type", {
  expect_error(pev_fcont(1), regexp = "^No method")
})

test_that("pev_fcont no-op for pev_fcont", {
  expect_identical(pev_fcont(fcont), fcont)
})

test_that("pev_fcont works for function", {
  fn <- unclass(fcont)
  expect_identical(pev_fcont(fn), fcont)
})

test_that("pev_fcont works for character", {
  expect_is(pev_fcont("Blues 2"), "pev_fcont")
})

test_that("pev_fcont works for pev_hcl", {
  hcl_qual <-
    pev_hcl(type = "qualitative", h1 = 0, c1 = 60, l1 = 60)

  expect_is(pev_fcont(hcl_qual), "pev_fcont")
})

test_that("pev_fcont gives us the right answers", {

  # qualitative
  pals_qual <- c("Pastel 1", "Warm")

  exp_qual <- function(pal) {

    fcont <- pev_fcont(pal)

    # note that we are travelling the trajectory differently from
    # how colorspace travels it - hence the contortions...
    if (identical(fcont(0), fcont(1))) {
      # cyclical
      vals <- seq(4, 1) / 4
    } else {
      # not cyclical
      vals <- seq(3, 0) / 3
    }

    expect_identical(
      fcont(vals),
      colorspace::qualitative_hcl(4, palette = !!pal)
    )
  }

  purrr::walk(pals_qual, exp_qual)

  # sequential
  pals_seq <- c("Blues 2", "Purple-Blue")

  exp_seq <- function(pal) {
    fcont <- pev_fcont(pal)

    expect_identical(
      fcont(c(0, 0.25, 0.5, 0.75, 1)),
      colorspace::sequential_hcl(5, palette = !!pal, rev = TRUE) # for some reason, order is reversed
    )
  }

  purrr::walk(pals_seq, exp_seq)

  # diverging
  pals_div <- c("Blue-Red", "Green-Brown")

  exp_div <- function(pal) {
    fcont <- pev_fcont(pal)

    expect_identical(
      fcont(c(0, 0.25, 0.5, 0.75, 1)),
      colorspace::diverging_hcl(5, palette =  !!pal)
    )
  }

  purrr::walk(pals_div, exp_div)

})
