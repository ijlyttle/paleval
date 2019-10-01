hcl_qual <-
  pev_hcl_param(type = "qualitative", h1 = 0, h2 = 360, c1 = 60, l1 = 60)

test_that("we can build an hcl param", {

  expect_is(hcl_qual, "pev_hcl_param")
  expect_named(
    hcl_qual,
    c("type", "h1", "h2", "c1", "cmax", "c2", "l1", "l2", "p1", "p2", "fixup")
  )
  expect_identical(hcl_qual$type, "qualitative")
  expect_identical(hcl_qual$h1, 0)
  expect_identical(hcl_qual$h2, 360)
  expect_identical(hcl_qual$c1, 60)
  expect_identical(hcl_qual$c2, 60)
  expect_identical(hcl_qual$l1, 60)
  expect_identical(hcl_qual$l2, 60)
  expect_identical(hcl_qual$p1, 1)
  expect_identical(hcl_qual$p2, 1)
  expect_identical(hcl_qual$cmax, 60)
  expect_identical(hcl_qual$fixup, TRUE)

})

test_that("we can print an hcl param", {

  expect_output(
    print(hcl_qual),
    "^HCL Parameters.*"
  )

})


test_that("we can build hcl params from a colorspace palette", {

  expect_error(
    pev_map_hcl_param(1),
    "Argument `hcl_palettes`"
  )

})
