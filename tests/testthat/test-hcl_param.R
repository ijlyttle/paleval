library("colorspace")

hcl_qual <-
  pev_hcl(type = "qualitative", h1 = 0, c1 = 60, l1 = 60)

hcl_seq <-
  pev_hcl(type = "sequential", h1 = 0, c1 = 60, l1 = 60)

hcl_div <-
  pev_hcl(type = "diverging", h1 = 0, c1 = 60, l1 = 60)

test_that("we can build an hcl param", {

  expect_is(hcl_qual, "pev_hcl")
  expect_named(
    hcl_qual,
    c("type", "h1", "h2", "c1", "c2", "l1", "l2", "p1", "p2", "cmax", "fixup")
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
  expect_identical(hcl_qual$cmax, NA_real_)
  expect_identical(hcl_qual$fixup, TRUE)

  expect_identical(hcl_seq$h2, 0)

  expect_identical(hcl_div$c2, 0)
})

test_that("we can print an hcl param", {

  expect_output(
    print(hcl_qual),
    "^HCL Parameters.*"
  )

})


test_that("we can build hcl params from a colorspace palette", {

  expect_error(
    pev_map_hcl(1),
    "Argument `hcl_palettes`"
  )

  # need a function that takes a list
  pal_names <- c("Pastel 1", "Grays", "Purple-Blue", "Blue-Red")

  hcl_pal <- hcl_palettes(palette = pal_names)

  hcl <- pev_map_hcl(hcl_pal)

  expect_is(hcl, "list")
  expect_named(hcl, pal_names)
  expect_identical(
    unname(purrr::map_chr(hcl, purrr::pluck, "type")),
    c("qualitative", "sequential", "sequential", "diverging")
  )

  purrr::walk(
    hcl,
    ~expect_is(.x, "pev_hcl")
  )

})
