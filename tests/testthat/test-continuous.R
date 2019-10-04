library("colorspace")
library("purrr")

fcont <- pev_fcont("Green-Brown")

test_that("rescale works", {

  fcont_rescale <- pev_fcont_rescale(fcont, c(0.25, 0.75))

  expect_identical(
    fcont_rescale(c(0, 0.5, 1)),
    fcont(c(0.25, 0.5, 0.75))
  )

  expect_identical(pev_rescale_diverging(0.5), c(0.25, 0.75))
  expect_identical(pev_rescale_diverging(1), c(0, 1))

})
