test_that("me works", {
  skip_unless_dev_env()

  me <- me()
  expect_equal(me$email, "admin@dido.fr")

  org <- me$organizations[[1]]

  expect_equal(org$title, "BSI")
  expect_equal(org$acronym, "bsi")
  expect_true((nchar(org$id) > 0))
})
