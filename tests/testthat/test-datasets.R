test_that("check list_datasets errors on missing param", {
  err <- rlang::catch_cnd(get_dataset())
  expect_s3_class(err, "error_bad_argument")
})

test_that("check list_datasets works", {
  skip_unless_dev_env()

  ds <- list_datasets()

  expect_s3_class(ds, "tbl")
})

test_that("check get_alerts works", {
  skip_unless_dev_env()

  alerts <- get_alerts()

  expect_s3_class(alerts, "tbl")
})

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
