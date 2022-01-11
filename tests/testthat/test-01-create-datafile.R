test_that("create datafiles works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds create datafiles works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  date_published <- format(Sys.time(), "%Y-%m-%dT00:00:00") # .000Z")

  created_df <- create_datafile(
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

test_that("create complete datafiles works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds create complete datafiles work",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- create_datafile(
    dataset = dataset,
    title = "didoscalim df create complete datafiles works",
    description = "description",
    file_name = "dido-csv-simple.csv",
    millesime = "2022-10",
    date_diffusion = "2022-10-10T08:00:00.000Z",
    published = "2022-10-10",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31"
  )

  expect_equal(datafile$data$datafile_metadata$title, "didoscalim df create complete datafiles works")
  expect_equal(datafile$data$datafile_metadata$description, "description")
  expect_equal(datafile$data$datafile_metadata$temporal_coverage_start, "2020-01-01T00:00:00.000Z")
  expect_equal(datafile$data$datafile_metadata$temporal_coverage_end, "2020-12-31T00:00:00.000Z")
  expect_equal(datafile$data$datafile_metadata$published, "2022-10-10T00:00:00.000Z")
  expect_equal(datafile$data$datafile_millesime_date_diffusion, "2022-10-10T08:00:00.000Z")
  expect_equal(datafile$data$datafile_millesime, "2022-10")
})

test_that("create datafiles warns when missing param", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds create datafiles warns when missing param",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_error(create_datafile(title = "test", description = "test", file_name = "test.csv"), "is missing")
  expect_error(create_datafile(dataset = "test", description = "test", file_name = "test.csv"), "pas du type attendu")
  expect_error(create_datafile(dataset = dataset, description = "test", file_name = "test.csv"), "is missing")
  expect_error(create_datafile(dataset = dataset, title = "test", file_name = "test.csv"), "is missing")
  expect_error(create_datafile(dataset = dataset, title = "test", description = "test"), "est obligatoire")
})
