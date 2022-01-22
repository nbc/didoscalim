test_that("add_dataset works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim check add_dataset works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, "didoscalim check add_dataset works")
  expect_equal(dataset$license, "fr-lo")
})

test_that("add_dataset fails correctly", {
  skip_unless_dev_env()

  err <- rlang::catch_cnd(
    add_dataset(
      title = "didoscalim ds add_dataset fails correctly",
      description = "test",
      topic = "Transports",
      frequency = "annual"
    )
  )

  expect_s3_class(err, "api_error")
  expect_match(err$message, "Erreur de validation")
})

test_that("add_dataset errors on missing param", {
  expect_error(add_dataset(description = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", description = "test", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", description = "test", topic = "Transports"), "is missing")
})
