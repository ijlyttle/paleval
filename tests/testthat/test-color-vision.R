name <- "Purple-Green"
n <-  11
fcont <- pev_fcont(name)
fdisc <- pev_fdisc(fcont, n = n)

test_that("cvd modification works", {

  ## bad class
  expect_error(pev_fpal_cvd(1), regexp = "^No method")

  ## discrete
  # none
  expect_identical(
    pev_fpal_cvd(fdisc, type = "none")(), # get all colors
    fdisc()
  )

  # deutan
  expect_identical(
    pev_fpal_cvd(fdisc, type = "deutan")(), # get all colors
    colorspace::deutan(colorspace::diverging_hcl(n = n, palette = name))
  )

  # protan
  expect_identical(
    pev_fpal_cvd(fdisc, type = "protan")(), # get all colors
    colorspace::protan(colorspace::diverging_hcl(n = n, palette = name))
  )

  # tritan
  expect_identical(
    pev_fpal_cvd(fdisc, type = "tritan")(), # get all colors
    colorspace::tritan(colorspace::diverging_hcl(n = n, palette = name))
  )

  ## continuous
  expect_identical(
    pev_fdisc(pev_fpal_cvd(fcont, type = "deutan"), n = n)(),
    colorspace::deutan(colorspace::diverging_hcl(n = n, palette = name))
  )

})
