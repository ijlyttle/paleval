good <- c("#000000", "#FFFFFF")
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

test_that("can remove alpha", {
  expect_identical(
    remove_alpha(c("#11223344", "#55667788", "#AABBCC")),
    c("#112233", "#556677", "#AABBCC")
  )
})

test_that("we can detect an RGB limit", {
  expect_identical(
    is_rgb_limit(c("#111111", "#001111", "#000011", "#1111FF")),
    c(FALSE, TRUE, TRUE, TRUE)
  )
})
