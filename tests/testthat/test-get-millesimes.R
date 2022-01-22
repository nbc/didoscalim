test_that("get_millesimes works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds get_millesimes works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- add_datafile(
    dataset = dataset,
    title = "didoscalim df get_millesimes works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  ml <- get_millesimes()
  expect_s3_class(ml, "tbl")
})
