name <- "Purple-Green"
n <-  11
fcont <- pev_fcont(name)
fdisc <- pev_fdisc(fcont, n = n)

test_that("to_hex works", {

  ## bad class
  expect_error(pev_fpal_to_hex(1), regexp = "^No method")

  # discrete
  expect_identical(
    pev_fpal_to_hex(fdisc),
    fdisc()
  )

  # continuous
  expect_identical(
    pev_fpal_to_hex(fcont, n = n),
    pev_fdisc(fcont, n = n)()
  )

})
