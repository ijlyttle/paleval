ex <- function(x, y) {
  expect_equal(!!x, !!y, tolerance = 1.e-5)
}

test_that("hex-distance works", {

  ex(
    pev_hex_distance("#000000", "#FFFFFF"),
    100
  )

  ex(
    pev_hex_distance(c("#000000", "#FFFFFF"), "#000000"),
    c(0, 100)
  )

  ex(
    pev_hex_distance("#000000", c("#000000", "#FFFFFF")),
    c(0, 100)
  )

  ex(
    pev_hex_distance(c("#000000", "#FFFFFF"), c("#000000", "#FFFFFF")),
    c(0, 0)
  )

})

test_that("hex-distance errors", {

  expect_error(pev_hex_distance("foo", "#FFFFFF"), regexp = "is_hexcolor")
  expect_error(pev_hex_distance("#FFFFFF", "foo"), regexp = "is_hexcolor")

  expect_error(
    pev_hex_distance("#000000", "#FFFFFF", method = "foo"),
    regexp = "method"
  )

  expect_error(
    pev_hex_distance(rep("#000000", 3), rep("#FFFFFF", 2)),
    regexp = "reconcile"
  )

})

test_that("hex-derivative works", {

  ex(
    pev_hex_derivative(c("#000000", "#777777", "#FFFFFF")),
    c(46.20568, 100.00000, 45.94851)
  )

})

test_that("hex-derivative errors", {

  expect_error(pev_hex_derivative("foo"), regexp = "is_hexcolor")

  expect_error(
    pev_hex_derivative(c("#000000", "#777777", "#FFFFFF"), method = "foo"),
    regexp = "method"
  )

})
