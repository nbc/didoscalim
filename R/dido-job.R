#' L'objet dido_job
#'
#' @description
#' L'objet dido_job est retourné par les fonctions `add_datafile()`,
#' `add_millesime()` et `replace_millesime()`, il peut être utilisé en argument
#' pour les fonctions `add_dataset()`, `add_datafile()`
#'
#' @name dido_job
NULL

#' @noRd
#' @export
dido_job <- function(x) {
  structure(x, class = c("dido_job", "list"))
}

is.dido_job <- function(x) inherits(x, "dido_job")

#' @export
print.dido_job <- function(x, ...) {
  str(x)
  invisible(x)
}

#' @export
get_dataset_id.dido_job <- function(data, ...) data$data$dataset_id

#' @export
get_datafile_rid.dido_job <- function(data, ...) data$result$rid
