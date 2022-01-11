#' @noRd
generate_random_string <- function() {
  as.character(runif(1))
}

skip_unless_dev_env <- function() {
  if (get_work_env() == "DEV") {
    return(invisible(TRUE))
  }
  testthat::skip("No dev env configured")
}
