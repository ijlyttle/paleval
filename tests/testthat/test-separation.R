library("colorspace")

good <- c("#000000", "#FFFFFF")
test_good <- pev_data_separation(good)
plot_good <- pev_gg_separation(test_good)

test_that("separation data works", {

  expect_is(test_good, "tbl_df")

  expect_identical(
    test_good$color_a,
    rep(good, each = length(good))
  )

  expect_identical(
    test_good$color_b,
    rep(good, times = length(good))
  )

  expect_equal(
    test_good$difference,
    c(0, 100, 100, 0),
    tolerance = 1.e-5
  )

})

test_that("separation plot works", {
  expect_is(plot_good, "ggplot")
})
