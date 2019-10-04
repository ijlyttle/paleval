library("colorspace")

good <- c("#000000", "#FFFFFF")
test_good <- pev_data_separation(good, method = "cie2000")
test_good_none <-
  pev_data_separation(good, method = "cie2000", include_cvd = FALSE)
plot_good <- pev_gg_separation(test_good)

test_that("separation data works", {

  expect_is(test_good, "tbl_df")

  expect_identical(
    names(test_good),
    c("cvd", "index_a", "hex_a", "hex_b", "distance")
  )

  expect_equal(
    test_good$distance,
    c(0, 100, 100, 0, 0, 99.75180, 99.75180, 0, 0, 99.97655, 99.97655, 0, 0, 100, 100, 0),
    tolerance = 1.e-5
  )

  expect_equal(
    test_good_none$distance,
    c(0, 100, 100, 0),
    tolerance = 1.e-5
  )



})

test_that("separation plot works", {
  expect_is(plot_good, "ggplot")
})

