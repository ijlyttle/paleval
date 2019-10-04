name <- "Purple-Green"
n <-  11
fcont <- pev_fcont(name)
fdisc <- pev_fdisc(fcont, n = n)

test_that("cvd modification works", {

  # discrete
  expect_identical(
    pev_fpal_cvd(fdisc, type = "deutan")(), # get all colors
    colorspace::deutan(colorspace::diverging_hcl(n = n, palette = name))
  )

  # continuous
  expect_identical(
    pev_fdisc(pev_fpal_cvd(fcont, type = "deutan"), n = n)(),
    colorspace::deutan(colorspace::diverging_hcl(n = n, palette = name))
  )
})
