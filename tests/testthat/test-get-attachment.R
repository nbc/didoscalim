test_that("get_attachment works", {
  skip_unless_dev_env()

  att_title <- glue::glue("didoscalim df get_attachment works {generate_random_string()}")
  dataset <- add_dataset(
    title = "didoscalim ds get_attachment works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  att <- add_attachment(
    dataset = dataset,
    title = att_title,
    description = "test",
    file_name = "dido-csv-simple.csv"
  )

  get_attachment(title = att_title, dataset = dataset) %>%
    expect_s3_class("dido_attachment")

  get_attachment(title = att_title, dataset = get_dataset_id(dataset)) %>%
    expect_s3_class("dido_attachment")

  get_attachment(att$rid, dataset = get_dataset_id(dataset)) %>%
    expect_s3_class("dido_attachment")
})
