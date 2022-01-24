test_that("delete works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds check get_dataset works",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31",
  )

  expect_true(delete_dataset(dataset$id), "delete ok")
})

test_that("check delete_dataset errors on missing param", {
  expect_error(delete_dataset(), "obligatoire")
})
