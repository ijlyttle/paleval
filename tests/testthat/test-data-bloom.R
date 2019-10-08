hex <- c("#000000", "#FFFFFF")
bloom_int <- .hcl(hex)
bloom <- pev_data_bloom(hex)

test_that("internal implemenation works", {

  expect_is(bloom_int, "data.frame")
  expect_named(bloom_int, c("x", "hue", "chroma", "luminance"))

  expect_identical(bloom_int$x, c(0, 1))
  expect_equal(bloom_int$hue, c(180, 94.1), tolerance = 0.1)
  expect_equal(bloom_int$chroma, c(0, 0.00769), tolerance = 1.e-5)
  expect_identical(bloom_int$luminance, c(0, 100))

})

test_that("hex implementation works", {

  expect_is(bloom, "data.frame")
  expect_named(bloom, c("cvd", "x", "hex", "hue", "chroma", "luminance"))

  expect_identical(
    bloom$cvd,
    rep(c("none", "deutan", "protan", "tritan"), each = 2)
  )

  expect_identical(bloom$x, rep(c(0, 1), times = 4))

  expect_equal(
    bloom$hue,
    c(180, 94.1, 180, 308, 180, 86, 180, 94.1),
    tolerance = 0.1
  )

  expect_equal(
    bloom$chroma,
    c(0, 0.00769, 0, 0.816, 0, 0.780, 0, 0.0069),
    tolerance = 0.1
  )

  expect_equal(
    bloom$luminance,
    c(0, 100, 0, 99.8, 0, 100, 0, 100),
    tolerance = 0.1
  )

})

test_that("bloom implementations are equivalent", {

  name_ref <- "Viridis"
  n_ref <- 13
  hex_ref <- pev_fcont(name_ref)(seq(0, 1, length.out = n_ref))
  bloom_ref <- pev_data_bloom(hex_ref)

  # continuous
  expect_identical(
    name_ref %>% pev_fcont() %>% pev_data_bloom(n = n_ref),
    bloom_ref
  )
  # discrete bounded
  expect_identical(hex_ref %>% pev_fdisc() %>% pev_data_bloom(), bloom_ref)
  # discrete unbounded
  expect_identical(
    name_ref %>% pev_fcont() %>% pev_fdisc() %>% pev_data_bloom(n = n_ref),
    bloom_ref
  )
  # name of colorspace palette
  expect_identical(name_ref %>% pev_data_bloom(n = n_ref), bloom_ref)

  # cvd = none
  expect_identical(
    name_ref %>% pev_data_bloom(n = n_ref, include_cvd = FALSE),
    bloom_ref[bloom_ref$cvd == "none", ]
  )

})
