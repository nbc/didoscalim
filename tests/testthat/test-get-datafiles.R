test_that("list_datafiles works", {
  skip_unless_dev_env()

  df <- list_datafiles()
  expect_s3_class(df, "tbl")
})
