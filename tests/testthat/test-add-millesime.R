test_that("add millesime works", {
  skip_unless_dev_env()

  dataset <- create_dataset(
    title = "didoscalim ds add millesime works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  job_datafile <- create_datafile(
    dataset = dataset,
    title = "didoscalim df add millesime works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  millesime <- add_millesime(
    datafile = get_datafile(get_datafile_rid(job_datafile)),
    millesime = "2022-10",
    file_name = "dido-csv-simple.csv"
  )

  expect_s3_class(millesime, "dido_job")
  expect_equal(millesime$data$datafile_millesime, "2022-10")

  df <- get_datafile(get_datafile_rid(job_datafile))
  expect_equal(df$millesimes, 2)
})

test_that("add_millesime errors on missing params", {
  expect_error(add_millesime(datafile = new_dido_datafile(list())), "obligatoire")
  expect_error(add_millesime(file_name = "dido-csv-simple.csv"), "obligatoire")
})
