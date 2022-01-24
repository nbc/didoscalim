test_that("check get_alerts works", {
  skip_unless_dev_env()

  alerts <- list_alerts()

  expect_s3_class(alerts, "tbl")
})
