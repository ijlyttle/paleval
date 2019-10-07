name <- "Purple-Green"
data_dist <- pev_data_distance(name)
data_dist_none <-pev_data_distance(name, include_cvd = FALSE)

g <- pev_gg_distance(data_dist)

test_that("hex-distance works", {

  expect_is(data_dist, "tbl_df")

  expect_identical(
    names(data_dist),
    c("cvd", "x", "hex", "hex_ref", "distance")
  )

  expect_identical(
    unique(data_dist$cvd),
    c("none", "deutan", "protan", "tritan")
  )
  expect_identical(
    unique(data_dist_none$cvd),
    "none"
  )

})

test_that("hex-distancee plot works", {
  expect_is(g, "ggplot")
})
