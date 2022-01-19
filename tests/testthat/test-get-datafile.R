test_that("get datafile works", {
  skip_unless_dev_env()

  title <- glue::glue("didoscalim rest datafiles {generate_random_string()}")

  dataset <- create_dataset(
    title = "didoscalim test datafiles",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- create_datafile(
    dataset = dataset,
    title = title,
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  df <- get_datafile(datafile)
  expect_equal(df$rid, datafile$result$rid)

  df <- get_datafile(datafile$result$rid)
  expect_equal(df$rid, datafile$result$rid)

  df <- get_datafile(title = title)
  expect_equal(df$rid, datafile$result$rid)
})
