test_that("update_datafile works", {
  skip_unless_dev_env()

  new_title <- "didoscalim df un nouveau titre"
  dataset <- create_dataset(
    title = "didoscalim ds update_datafile works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- create_datafile(
    dataset = dataset,
    title = "didoscalim df update_datafile works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  updated <- get_datafile(datafile$result$rid)

  updated$temporal_coverage$start <- "2023-01-01"
  updated$temporal_coverage$end <- "2023-12-31"
  updated$title <- new_title

  df <- update_datafile(updated)

  expect_equal(df$temporal_coverage$end, "2023-12-31T00:00:00.000Z")
  expect_equal(df$title, new_title)
})

test_that("update_datafiles errors on missing params", {
  expect_error(update_datafile(), "obligatoire")
  expect_error(update_datafile(datafile = "id"), "pas du type attendu")
})
