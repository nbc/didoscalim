test_that("csv function works for default locale", {
  tbl <- dido_read_delim(paste0(test_path(), "/example-default.csv"))

  params <- list(
    DESCRIPTION = list(description = "Une description"),
    UNIT_MWH = list(unit = "MWh"),
    TYPE_NAF = list(type = "naf_division")
  )
  result <- dido_csv(tbl, params = params)

  expected <- read_delim(paste0(test_path(), "/example-default-result.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_equal(result, expected)
})

test_that("csv functions work with ISO-8859-15", {
  locale <- readr::locale(encoding = "ISO-8859-15")

  tbl <- dido_read_delim(
    paste0(test_path(), "/example-iso-8859-15.csv"),
    locale = locale
  )

  params <- list(
    DESCRIPTION = list(description = "Une description"),
    UNIT_MWH = list(unit = "MWh"),
    TYPE_NAF = list(type = "naf_division")
  )
  result <- dido_csv(
    tbl,
    params = params,
    locale = locale
  )

  expected <- read_delim(
    paste0(test_path(), "/example-default-result.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_equal(result, expected)
})

test_that("csv functions work with comma", {
  locale <- readr::locale(decimal_mark = ",")
  tbl <- dido_read_delim(
    paste0(test_path(), "/example-comma.csv"),
    locale = locale
  )

  params <- list(
    DESCRIPTION = list(description = "Une description"),
    UNIT_MWH = list(unit = "MWh"),
    TYPE_NAF = list(type = "naf_division")
  )
  result <- dido_csv(
    tbl,
    params = params,
    locale = locale
  )

  expected <- read_delim(
    paste0(test_path(), "/example-comma-result.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_equal(result, expected)
})

test_that("csv functions work with ISO-8859-15 and comma", {
  locale <- readr::locale(
    encoding = "ISO-8859-15",
    decimal_mark = ","
  )

  tbl <- dido_read_delim(
    paste0(test_path(), "/example-iso-8859-15-comma.csv"),
    locale = locale
  )

  params <- list(
    DESCRIPTION = list(description = "Une description"),
    UNIT_MWH = list(unit = "MWh"),
    TYPE_NAF = list(type = "naf_division")
  )
  result <- dido_csv(
    tbl,
    params = params,
    locale = locale
  )

  expected <- read_delim(
    paste0(test_path(), "/example-comma-result.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_equal(result, expected)
})
