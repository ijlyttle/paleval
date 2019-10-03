library("colorspace")
library("purrr")

test_that("qualitative works", {

  name <- "Pastel 1"
  hcl_param <-
    name %>%
    hcl_palettes(palette = .) %>%
    pev_map_hcl_param() %>%
    pluck(1)

  fcont <- pev_fcont(hcl_param)

  expect_identical(
    fcont(c(0, 0.5)),
    qualitative_hcl(2, palette = name)
  )

})

test_that("sequential works", {

  name <- "Purple-Blue"
  hcl_param <-
    name %>%
    hcl_palettes(palette = .) %>%
    pev_map_hcl_param() %>%
    pluck(name)

  fcont <- pev_fcont(hcl_param)

  expect_identical(
    fcont(c(0, 0.5, 1)),
    sequential_hcl(3, palette = name, rev = TRUE) # for some reason, order is reversed
  )

})


test_that("diverging works", {

  name <- "Green-Brown"
  hcl_param <-
    name %>%
    hcl_palettes(palette = .) %>%
    pev_map_hcl_param() %>%
    pluck(name)

  fcont <- pev_fcont(hcl_param)

  expect_identical(
    fcont(c(0, 0.25, 0.5, 0.75, 1)),
    diverging_hcl(5, palette = name)
  )

})

test_that("reverse works", {

  name <- "Green-Brown"
  hcl_param <-
    name %>%
    hcl_palettes(palette = .) %>%
    pev_map_hcl_param() %>%
    pluck(name)

  fcont <- pev_fcont_reverse(pev_fcont(hcl_param))

  expect_identical(
    fcont(c(0, 0.25, 0.5, 0.75, 1)),
    diverging_hcl(5, palette = name, rev = TRUE)
  )

})

test_that("rescale works", {

  name <- "Green-Brown"
  hcl_param <-
    name %>%
    hcl_palettes(palette = .) %>%
    pev_map_hcl_param() %>%
    pluck(name)

  fcont <- pev_fcont_rescale(pev_fcont(hcl_param), c(0.25, 0.75))

  expect_identical(
    fcont(c(0, 0.5, 1)),
    diverging_hcl(5, palette = name)[2:4]
  )

  expect_identical(pev_rescale_div(0.5), c(0.25, 0.75))
  expect_identical(pev_rescale_div(1), c(0, 1))

})
