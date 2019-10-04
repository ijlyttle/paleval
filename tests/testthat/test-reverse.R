name <- "Purple-Green"
n <-  11
fcont <- pev_fcont(name)
fdisc <- pev_fdisc(fcont, n = n)

test_that("reverse works", {

  ## bad class
  expect_error(pev_fpal_reverse(1), regexp = "^No method")

  # discrete
  expect_identical(
    pev_fpal_reverse(fdisc)(),
    rev(fdisc())
  )

  # continuous
  expect_identical(
    pev_fdisc(pev_fpal_reverse(fcont), n = n)(),
    rev(pev_fdisc(fcont, n = n)())
  )

})
