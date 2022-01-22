test_that("abort_bad_argument works", {
  expect_error(abort_bad_argument("arg"),
               "`arg` est obligatoire et ne peut être null",
               class = "error_bad_argument")
})

