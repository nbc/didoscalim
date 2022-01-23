test_that("get_attachments works", {
  skip_unless_dev_env()

  df <- get_attachments()
  expect_s3_class(df, "tbl")
})

test_that("get_attachments works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds get_attachments works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  df <- get_attachments(dataset = dataset)
  expect_equal(nrow(df), 0)

  att <- add_attachment(
    dataset = dataset,
    title = "didoscalim df add_attachment works",
    description = "test",
    file_name = "dido-csv-simple.csv"
  )

  df <- get_attachments(dataset = dataset)
  expect_equal(nrow(df), 1)
})

