test_that("hex-distance works", {

  ex <- function(x, y) {
    expect_equal(!!x, !!y, tolerance = 1.e-5)
  }

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
