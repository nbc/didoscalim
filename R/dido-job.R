#' L'objet dido_job
#'
#' @description
#' L'objet dido_job est retourné par les fonctions `create_datafile()`,
#' `add_millesime()` et `replace_millesime()`
#'
#' @name dido_job
NULL

#' @noRd
#' @export
dido_job <- function(x) {
  structure(x, class = c("dido_job", class(x)))
}

is.dido_job <- function(x) inherits(x, "dido_job")

print.dido_job <- function(x, ...) {
  str(x)
  invisible(x)
}

get_id.dido_job <- function(x, ...) x$data$dataset_id

get_datafile_rid.dido_job <- function(x, ...) x$result$rid
