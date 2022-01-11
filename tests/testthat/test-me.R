test_that("me works", {
  skip_unless_dev_env()

  me <- me()
  expect_equal(me$email, "admin@dido.fr")

  org <- me$organizations[[1]]

  expect_equal(org$title, "BSI")
  expect_equal(org$acronym, "bsi")
  expect_true((nchar(org$id) > 0))
})

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
  expect_error(organization("some random string"), "aucune organisation")
})
