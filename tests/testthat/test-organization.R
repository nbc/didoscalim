test_that("organization works without params", {
  skip_unless_dev_env()

  org <- organization()

  expect_true((nchar(org) > 0))
})

test_that("organization works with good params", {
  skip_unless_dev_env()

  org <- organization("bsi")

  expect_true((nchar(org) > 0))
})

test_that("organization errors if no organization found", {
  skip_unless_dev_env()

  expect_error(organization("some random string"), "aucune organisation")
})
