test_that("extent works", {
  expect_identical(
    extent(55, increment = 20),
    60
  )

  expect_identical(
    extent(seq(1, 55), increment = 20),
    60
  )
})
