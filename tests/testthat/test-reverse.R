name <- "Purple-Green"
n <-  11
fcont <- pev_fcont(name)
fdisc_fn <- pev_fdisc(fcont)
fdisc_hex <- pev_fdisc(fdisc_fn(n))

test_that("reverse works", {

  # bad class
  expect_error(pev_fcont_reverse(1), regexp = "^No method")
  expect_error(pev_fdisc_reverse(1), regexp = "^No method")

  # discrete
  expect_identical(
    pev_fdisc_reverse(fdisc_fn)(n),
    rev(fdisc_fn(n))
  )

  expect_identical(
    pev_fdisc_reverse(fdisc_hex)(n),
    rev(fdisc_hex(n))
  )

  # continuous
  expect_identical(
    pev_fdisc(pev_fcont_reverse(fcont))(n),
    rev(pev_fdisc(fcont)(n))
  )

})
