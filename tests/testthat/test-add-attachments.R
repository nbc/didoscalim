test_that("add_attachment works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds add_attachment works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  att <- add_attachment(
    dataset = dataset,
    title = "didoscalim df add_attachment works",
    description = "test",
    file_name = "dido-csv-simple.csv"
  )

  expect_equal(att$title, "didoscalim df add_attachment works")
  expect_equal(att$description, "test")
})

test_that("add_attachments errors on missing param", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds add_attachments errors on missing param",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_error(add_attachment(title = "test", description = "test", file_name = "test.csv"), "obligatoire")
  expect_error(add_attachment(dataset = dataset, description = "test", file_name = "test.csv"), "obligatoire")
  expect_error(add_attachment(dataset = dataset, title = "test", file_name = "test.csv"), "obligatoire")
  expect_error(add_attachment(dataset = dataset, title = "test", description = "test"), "obligatoire")
})
