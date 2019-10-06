name <- "Purple-Green"
n <-  11
fcont <- pev_fcont(name)
fdisc <- pev_fdisc(fcont, n = n)

test_that("cvd modification works", {

  ## bad class
  expect_error(pev_fdisc_cvd(1), regexp = "^No method")
  expect_error(pev_fcont_cvd(1), regexp = "^No method")

  ## discrete
  # none
  expect_identical(
    pev_fdisc_cvd(fdisc, type = "none")(n), # get all colors
    fdisc(n)
  )

  # deutan
  expect_identical(
    pev_fdisc_cvd(fdisc, type = "deutan")(n), # get all colors
    colorspace::deutan(colorspace::diverging_hcl(n = n, palette = name))
  )

  # protan
  expect_identical(
    pev_fdisc_cvd(fdisc, type = "protan")(n), # get all colors
    colorspace::protan(colorspace::diverging_hcl(n = n, palette = name))
  )

  # tritan
  expect_identical(
    pev_fdisc_cvd(fdisc, type = "tritan")(n), # get all colors
    colorspace::tritan(colorspace::diverging_hcl(n = n, palette = name))
  )

  ## continuous
  expect_identical(
    pev_fdisc(pev_fcont_cvd(fcont, type = "deutan"))(n), # get all colors
    colorspace::deutan(colorspace::diverging_hcl(n = n, palette = name))
  )

})
