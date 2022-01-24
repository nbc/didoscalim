test_that("check list_datasets errors on missing param", {
  err <- rlang::catch_cnd(get_dataset())
  expect_s3_class(err, "error_bad_argument")
})

test_that("check list_datasets works", {
  skip_unless_dev_env()

  ds <- list_datasets()

  expect_s3_class(ds, "tbl")
})
