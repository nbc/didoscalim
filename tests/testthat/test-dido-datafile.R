test_that("dido_datafile works", {
  dataset <- create_dataset(
    title = "didoscalim ds dido_datafile work",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- dido_datafile(
    dataset = dataset,
    title = "didoscalim df create datafiles work",
    description = "description"
  )
  expect_equal(datafile$title, "didoscalim df create datafiles work")
  expect_equal(datafile$description, "description")
  expect_equal(datafile$published, format(Sys.time(), "%Y-%m-%d"))
  expect_equal(datafile$millesime, format(Sys.time(), "%Y-%m"))
})

test_that("commplete dido_datafile works", {
  dataset <- create_dataset(
    title = "didoscalim ds complete create datafile work",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- dido_datafile(
    dataset = dataset,
    title = "didoscalim df create complete datafiles works",
    description = "description",
    millesime = "2022-10",
    date_diffusion = "2022-10-10T08:00:00.000Z",
    published = "2022-10-10",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31"
  )

  expect_equal(datafile$title, "didoscalim df create complete datafiles works")
  expect_equal(datafile$description, "description")
  expect_equal(datafile$temporal_coverage_start, "2020-01-01")
  expect_equal(datafile$temporal_coverage_end, "2020-12-31")
  expect_equal(datafile$published, "2022-10-10")
  expect_equal(datafile$date_diffusion, "2022-10-10T08:00:00.000Z")
  expect_equal(datafile$millesime, "2022-10")
})
