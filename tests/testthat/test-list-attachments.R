test_that("list_attachments works", {
  skip_unless_dev_env()

  df <- list_attachments()
  expect_s3_class(df, "tbl")
})

test_that("list_attachments works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds list_attachments works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  df <- list_attachments(dataset = dataset)
  expect_equal(nrow(df), 0)

  att <- add_attachment(
    dataset = dataset,
    title = "didoscalim df add_attachment works",
    description = "test",
    file_name = "dido-csv-simple.csv"
  )

  df <- list_attachments(dataset = dataset)
  expect_equal(nrow(df), 1)
})

