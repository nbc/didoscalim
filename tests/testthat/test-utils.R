test_that("abort_bad_argument works", {
  expect_error(abort_bad_argument("arg"), "`arg` est obligatoire et ne peut être null")
})

test_that("abort_bad_argument_type works", {
  err <- rlang::catch_cnd(abort_bad_argument_type("arg", c("fun1", "fun2")))

  expect_s3_class(err, "error_bad_argument_type")
})
