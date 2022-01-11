test_that("get_datafiles works", {
  skip_unless_dev_env()

  df <- get_datafiles()
  expect_s3_class(df, "tbl")
})

test_that("get datafile errors on missing params", {
  err <- rlang::catch_cnd(get_datafile())

  expect_s3_class(err, "error_bad_argument")
})
