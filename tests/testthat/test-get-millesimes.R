test_that("list_millesimes works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds list_millesimes works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- add_datafile(
    dataset = dataset,
    title = "didoscalim df list_millesimes works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  ml <- list_millesimes()
  expect_s3_class(ml, "tbl")
})
