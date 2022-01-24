test_that("my_organization works without params", {
  skip_unless_dev_env()

  org <- my_organization()

  expect_true((nchar(org) > 0))
})

test_that("my_organization works with good params", {
  skip_unless_dev_env()

  org <- my_organization("bsi")

  expect_true((nchar(org) > 0))
})

test_that("my_organization errors if no organization found", {
  skip_unless_dev_env()

  expect_error(my_organization("some random string"), "aucune organisation")
})
