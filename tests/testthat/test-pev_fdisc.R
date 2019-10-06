name <- "Blues 2"
n <- 5

fcont <- pev_fcont(name)
hex <- colorspace::sequential_hcl(n = n, palette = name)

fdisc_hex <- pev_fdisc(hex)
fdisc_fn <- pev_fdisc(pev_fcont(name), n = n, method = "post")

test_that("pev_disc throws error for unknown type", {
  expect_error(pev_fdisc(1), regexp = "^No method")
})

test_that("pev_fdisc no-op for pev_disc", {
  expect_identical(pev_fdisc(fdisc_hex), fdisc_hex)
})

test_that("pev_fdisc works for hex", {
  expect_is(fdisc_hex, "pev_fdisc")
})

test_that("pev_fdisc works for fcont", {
  expect_is(fdisc_fn, "pev_fdisc")
})

test_that("pev_fdisc gives the right answers", {

  # call for each index
  purrr::walk(
    seq_along(hex),
    ~expect_identical(fdisc_hex(.x), hex[seq_len(.x)])
  )

  # call with too-big index
  expect_identical(
    fdisc_hex(length(hex) + 1),
    c(fdisc_hex(length(hex)), NA_character_)
  )

  # we discretize correctly
  vals_post <- c(0, 0.25, 0.5, 0.75, 1.0)
  vals_panel <- c(0.1, 0.3, 0.5, 0.7, 0.9)

  fdisc_post <- pev_fdisc(fcont, method = "post")
  fdisc_panel <- pev_fdisc(fcont, method = "panel")

  expect_identical(fdisc_post(5), fcont(vals_post))
  expect_identical(fdisc_panel(5), fcont(vals_panel))

})

test_that("pev_nmax works", {
  expect_identical(pev_nmax(fdisc_hex), n)
  expect_identical(pev_nmax(fdisc_fn), Inf)
})

test_that("pev_fdisc prints", {
  # this is frustrating because I can't see how to test that a plot
  # is produced
  expect_silent(print(fdisc_fn))
  expect_silent(print(fdisc_hex))
})
