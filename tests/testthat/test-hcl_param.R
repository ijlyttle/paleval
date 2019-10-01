library("colorspace")

hcl_qual <-
  pev_hcl_param(type = "qualitative", h1 = 0, c1 = 60, l1 = 60)

hcl_seq <-
  pev_hcl_param(type = "sequential", h1 = 0, c1 = 60, l1 = 60)

hcl_div <-
  pev_hcl_param(type = "diverging", h1 = 0, c1 = 60, l1 = 60)

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

  expect_identical(hcl_seq$h2, 0)

  expect_identical(hcl_div$c2, NA_real_)
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

  # need a function that takes a list
  pal_names <- c("Pastel 1", "Grays", "Purple-Blue", "Blue-Red")

  hcl_pal <- hcl_palettes(palette = pal_names)

  hcl_param <- pev_map_hcl_param(hcl_pal)

  expect_is(hcl_param, "list")
  expect_named(hcl_param, pal_names)
  expect_identical(
    unname(purrr::map_chr(hcl_param, purrr::pluck, "type")),
    c("qualitative", "sequential", "sequential", "diverging")
  )

  purrr::walk(
    hcl_param,
    ~expect_is(.x, "pev_hcl_param")
  )

})
