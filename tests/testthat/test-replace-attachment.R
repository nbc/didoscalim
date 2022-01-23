test_that("add_attachment works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
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

  replace_attachment(
    attachment = att,
    file_name = "file-upload.txt"
  ) %>%
    expect_s3_class("dido_attachment")
})

