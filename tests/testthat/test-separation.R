library("colorspace")

pal_discrete <- qualitative_hcl(11, palette = "pastel1")

test_that("separation data works", {
  expect_is(
    pev_data_separation(pal_discrete),
    "tbl_df"
  )
})
