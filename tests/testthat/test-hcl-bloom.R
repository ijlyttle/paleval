test_that("bloom ggplot works", {
  data_bloom <- pev_data_hcl("Viridis")
  data_bloom_ref <- pev_data_hcl_ref("Viridis", "#008F97")
  expect_is(pev_gg_hcl_bloom_target(data_bloom, data_bloom_ref), "gg")
  expect_is(pev_gg_hcl_bloom_lum(data_bloom, data_bloom_ref), "gg")
  expect_is(pev_gg_hcl_bloom(data_bloom, data_bloom_ref), "gg")
  expect_is(pev_gg_hcl_bloom(data_bloom, data_bloom_ref, label = TRUE), "gg")
})
