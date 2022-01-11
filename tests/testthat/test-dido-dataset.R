test_that("multiplication works", {
  dataset <- dido_dataset(
    title = "check update_dataset works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, "check update_dataset works")
  expect_equal(dataset$license, "fr-lo")
})

test_that("complete create_dataset works", {
dataset <- dido_dataset(
  title = "didoscalim ds complete create_dataset works",
  description = "test",
  topic = "Transports",
  tags = list("agriculture"),
  frequency = "annual",
  frequency_date = "2021-01-01",
  granularity = "fr:region",
  zones = list("country:fr"),
  license = "ODbL-1.0",
  temporal_coverage_start = "2020-01-01",
  temporal_coverage_end = "2020-12-31",
  caution = "Some text"
)

expect_equal(dataset$title, "didoscalim ds complete create_dataset works")
expect_equal(dataset$description, "test")
expect_equal(dataset$topic, "Transports")
expect_equal(dataset$tags, list("agriculture"))
expect_equal(dataset$frequency, "annual")
expect_equal(dataset$frequency_date, "2021-01-01")
expect_equal(dataset$spatial$granularity, "fr:region")
expect_equal(dataset$spatial$zones, list("country:fr"))
expect_equal(dataset$license, "ODbL-1.0")
expect_equal(dataset$caution, "Some text")

})
