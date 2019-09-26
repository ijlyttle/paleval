good <- c("#12ab34", "#FFFFFF")
bad <- c("#p2ab34", "#FFFFFF")

list_good <- as.list(good)
names_good <- c("a", "b")

test_that("can distinguish hexcolor", {
  expect_true(is_hexcolor(good))
  expect_false(is_hexcolor(bad))
})

test_that("can validate hexcolor", {

  expect_identical(as_hexcolor(good), good)
  expect_identical(as_hexcolor(list_good), good)

  expect_error(as_hexcolor(bad), "hexcolor")
})
