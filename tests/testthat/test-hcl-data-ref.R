hrs <- .hex_hcl_ref_single(
  hex_ref = "#000000",
  hex = c("#000000", "#FFFFFF")
)

hrs_1 <- .hex_hcl_ref_single(
  hex_ref = "#FFFFFF",
  hex = c("#000000", "#FFFFFF")
)

href <- .hex_hcl_ref(
  hex_ref = c("#000000", "#FFFFFF"),
  hex = c("#000000", "#FFFFFF")
)

test_that(".hex_hcl_ref_single works", {

  expect_is(hrs, "data.frame")
  expect_named(
    hrs,
    c("cvd",
      "x_nearest",
      "distance_nearest",
      "hex_nearest",
      "hex_ref",
      "hue_ref",
      "chroma_ref",
      "luminance_ref"
    )
  )
  expect_identical(hrs$cvd, get_cvd())
  expect_identical(hrs$distance_nearest, rep(0, 4))
  expect_identical(hrs$hex_nearest, rep("#000000", 4))
  expect_identical(hrs$hex_ref, rep("#000000", 4))

  expect_identical(hrs_1$distance_nearest, rep(0, 4))
  expect_identical(hrs_1$hex_nearest, c("#FFFFFF","#FFFEFF","#FFFFFE","#FFFFFF"))
  expect_identical(hrs_1$hex_ref, c("#FFFFFF","#FFFEFF","#FFFFFE","#FFFFFF"))

})

test_that(".hex_hcl_ref works", {
  expect_identical(href, rbind(hrs, hrs_1))
})


test_that(".hex_hcl_ref works", {
  href <- .hex_hcl_ref(
    hex_ref = c("#000000", "#FFFFFF"),
    hex = c("#000000", "#FFFFFF")
  )

  expect_identical(href, rbind(hrs, hrs_1))
})

test_that("pev_data_hcl_ref works", {

  expect_identical(
    pev_data_hcl_ref(
      pev_fdisc(c("#000000", "#FFFFFF")),
      c("#000000", "#FFFFFF")
    ),
    href
  )

  expect_identical(
    pev_data_hcl_ref(
      pev_fcont(c("#000000", "#FFFFFF")),
      c("#000000", "#FFFFFF")
    ),
    href
  )


  expect_identical(
    pev_data_hcl_ref(
      pev_fdisc(pev_fcont(c("#000000", "#FFFFFF"))),
      c("#000000", "#FFFFFF")
    ),
    href
  )
})


