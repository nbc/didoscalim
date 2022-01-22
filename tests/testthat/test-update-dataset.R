test_that("check update_dataset works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds check update_dataset works",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$temporal_coverage$end, "2020-12-31")

  dataset$temporal_coverage$end <- "2023-12-31"
  updated_ds <- update_dataset(dataset)

  expect_equal(updated_ds$temporal_coverage$end, "2023-12-31")
})

test_that("update_dataset errors on param problems", {
  expect_error(update_dataset(), "obligatoire")
  expect_error(update_dataset(dataset = "id"), "pas du type attendu")
})
