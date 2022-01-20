test_that("create datafiles works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds create datafiles works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  date_published <- format(Sys.time(), "%Y-%m-%dT00:00:00") # .000Z")

  created_df <- add_datafile(
    dataset = dataset,
    title = "didoscalim df create datafiles work",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  datafile <- get_datafile(created_df$result$rid)

  expect_equal(datafile$title, "didoscalim df create datafiles work")
  expect_equal(datafile$description, "description")
  expect_equal(datafile$published, date_published)
})

test_that("create datafiles warns when missing param", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds create datafiles warns when missing param",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_error(add_datafile(title = "test", description = "test", file_name = "test.csv"), "is missing")
  expect_error(add_datafile(dataset = "test", description = "test", file_name = "test.csv"), "is missing")
  expect_error(add_datafile(dataset = dataset, description = "test", file_name = "test.csv"), "is missing")
  expect_error(add_datafile(dataset = dataset, title = "test", file_name = "test.csv"), "is missing")
  expect_error(add_datafile(dataset = dataset, title = "test", description = "test"), "est obligatoire")
})
