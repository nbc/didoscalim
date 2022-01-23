test_that("get_datafiles works", {
  skip_unless_dev_env()

  df <- get_datafiles()
  expect_s3_class(df, "tbl")
})
