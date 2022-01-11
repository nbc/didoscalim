test_that("check get_datasets errors on missing param", {
  err <- rlang::catch_cnd(get_dataset())
  expect_s3_class(err, "error_bad_argument")
})

test_that("check get_datasets works", {
  ds <- get_datasets()

  expect_s3_class(ds, "tbl")
})

test_that("check get_alerts works", {
  alerts <- get_alerts()

  expect_s3_class(alerts, "tbl")
})

test_that("check get_dataset works", {
  title <- glue::glue("didoscalim test get dataset {generate_random_string()}")

  ds <- create_dataset(
    title = title,
    description = "test",
    topic = "Transports",
    frequency = "unknown",
  )
  dataset <- get_dataset(ds$id)

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, title)
  expect_equal(dataset$description, "test")
  expect_equal(dataset$topic, "Transports")

  dataset <- get_dataset(title = title)

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, title)
  expect_equal(dataset$description, "test")
  expect_equal(dataset$topic, "Transports")
})

test_that("c", {
  dataset <- create_dataset(
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
