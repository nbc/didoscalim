test_that("upload files works", {
  skip_unless_dev_env()

  id <- upload_file(paste0(test_path(), "/file-upload.txt"))

  expect_true(nchar(id) > 0)
})

test_that("check_csv return error on txt", {
  skip_unless_dev_env()

  id <- upload_file(paste0(test_path(), "/file-upload.txt"))

  err <- rlang::catch_cnd(check_csv(id))
  expect_s3_class(err, "invalid_file")
})

test_that("check_csv works with valid file", {
  skip_unless_dev_env()

  id <- upload_file(paste0(test_path(), "/dido-csv-valid.csv"))

  result <- check_csv(id)
  expect_true(result)
})

test_that("check_csv fails on errors", {
  skip_unless_dev_env()

  id <- upload_file(paste0(test_path(), "/dido-csv-with-error.csv"))

  err <- rlang::catch_cnd(check_csv(id))
  expect_s3_class(err, "invalid_file")
})

test_that("check_csv works on warnings", {
  skip_unless_dev_env()

  id <- upload_file(paste0(test_path(), "/dido-csv-with-warning.csv"))

  expect_warning(check_csv(id), "Le fichier est valide mais il y a des alertes")
})

test_that("upload_file fails on missing param", {
  err <- rlang::catch_cnd(upload_file())

  expect_s3_class(err, "error_bad_argument")
  expect_match(err$message, "`file_name` est obligatoire et ne peut être null")
})

test_that("upload_file fails on missing file", {
  err <- rlang::catch_cnd(upload_file(paste0(test_path(), "/no_such_file.csv")))
  expect_s3_class(err, "no_such_file")
})
