test_that("basic create_dataset works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "check update_dataset works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, "check update_dataset works")
  expect_equal(dataset$license, "fr-lo")
})

test_that("complete create_dataset works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds complete create_dataset works",
    description = "test",
    topic = "Transports",
    tags = list("agriculture"),
    frequency = "annual",
    frequency_date = "2021-01-01",
    granularity = "fr:region",
    zones = list("country:fr"),
    license = "ODbL-1.0",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31",
    caution = "Some text"
  )

  expect_equal(dataset$title, "didoscalim ds complete create_dataset works")
  expect_equal(dataset$description, "test")
  expect_equal(dataset$topic, "Transports")
  expect_equal(dataset$tags, list("agriculture"))
  expect_equal(dataset$frequency, "annual")
  expect_equal(dataset$frequency_date, "2021-01-01T00:00:00+00:00")
  expect_equal(dataset$spatial, list("granularity" = "fr:region", "zones" = list("country:fr")))
  expect_equal(dataset$license, "ODbL-1.0")
  expect_equal(dataset$caution, "Some text")
})

test_that("create_dataset fails correctly", {
  err <- rlang::catch_cnd(
    create_dataset(
      title = "didoscalim ds create_dataset fails correctly",
      description = "test",
      topic = "Transports",
      frequency = "annual"
    )
  )

  expect_s3_class(err, "api_error")
  expect_match(err$message, "Erreur de validation")
})

test_that("create_dataset errors on missing param", {
  expect_error(create_dataset(description = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(create_dataset(title = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(create_dataset(title = "test", description = "test", frequency = "unknown"), "is missing")
  expect_error(create_dataset(title = "test", description = "test", topic = "Transports"), "is missing")
})
