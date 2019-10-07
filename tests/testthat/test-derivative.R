data_drv <- pev_data_derivative(pev_fcont("Purple-Green"))
g <- pev_gg_derivative(data_drv)

test_that("hex-derivative works", {

  expect_is(data_drv, "tbl_df")

  expect_identical(
    names(data_drv),
    c("cvd", "x", "hex", "d_distance_d_x")
  )

})

test_that("hex-derivative plot works", {
  expect_is(g, "ggplot")
})
