test_that("wait_for_jobs works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds wait_for_jobs works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile_job <- add_datafile(
    dataset = dataset,
    title = "didoscalim df wait_for_jobs works",
    description = "description",
    file = "dido-csv-simple.csv"
  )

  result <- wait_for_job(datafile_job$id)

  expect_s3_class(result, "dido_job")
  expect_true("result" %in% names(result))
  expect_true("data" %in% names(result))
})

test_that("get_jobs works", {
  skip_unless_dev_env()

  jobs <- get_jobs()

  expect_s3_class(jobs, "tbl")
})
