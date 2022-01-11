test_that("check get_dataset works", {
  skip_unless_dev_env()

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
