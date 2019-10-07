data_drv <- pev_data_derivative(pev_fcont("Purple-Green"))
data_drv_none <-
  pev_data_derivative(pev_fcont("Purple-Green"), include_cvd = FALSE)

g <- pev_gg_derivative(data_drv)

test_that("hex-derivative works", {

  expect_is(data_drv, "tbl_df")

  expect_identical(
    names(data_drv),
    c("cvd", "x", "hex", "d_distance_d_x")
  )

  expect_identical(
    unique(data_drv$cvd),
    c("none", "deutan", "protan", "tritan")
  )
  expect_identical(
    unique(data_drv_none$cvd),
    "none"
  )

})

test_that("hex-derivative plot works", {
  expect_is(g, "ggplot")
})
