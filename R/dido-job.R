#' L'objet dido_job
#'
#' @description
#' L'objet dido_job est retourné par les fonctions `add_datafile()`,
#' `add_millesime()` et `replace_millesime()`, il peut être utilisé en argument
#' pour les fonctions `add_dataset()`, `add_datafile()`
#'
#' Il a les champs suivant :
#'
#' * `owner` : l'id du propriétaire du job
#' * `state` l'état du job. Devrait être "complete" ou "failed"
#' * `data` les informations liées à la tâche, le champ `dataset_id`
#' * `result` : le résultat qui comprend lui même les champs :
#'   * `rid` l'identifiant du fichier de données
#'   * `duration` le temps de traitement
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
