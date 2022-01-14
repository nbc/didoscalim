#' Retourne un objet avec uniquement ses métadonnées propre
#'
#' @param data un objet dido_dataset, dido_datafile
#'
#' @return un objet réduit à ses propres métadonnées
#' @export
#'
#' @examples
#' \dontrun{
#' extract_id(dataset)
#' }
extract_metadata <- function(data) UseMethod("extract_metadata")

#' Prépare un objet pour l'envoyer à DiDo
#'
#' @param data objet dido_dataset, dido_datafile
#'
#' @return un objet nettoyé
#' @export
#'
#' @examples
#' \dontrun{
#' clean_metadata(dataset)
#' }
#' @keywords internal
clean_metadata <- function(data) UseMethod("clean_metadata")

#' Retourne le dataset id de l'objet
#'
#' @param data un objet dido_dataset, dido_datafile ou dido_job
#'
#' @return le dataset id
#' @export
#'
#' @examples
#' \dontrun{
#' get_id(dataset)
#' }
#' @keywords internal
get_id <- function(data) UseMethod("get_id")

get_id.default <- function(x) NULL

#' Retourne le dafile id de l'objet
#'
#' @param data un objet dido_datafile ou dido_job
#'
#' @return le datafile id
#' @export
#'
#' @examples
#' \dontrun{
#' get_datafile_rid(job)
#' }
#' @keywords internal
get_datafile_rid <- function(data) UseMethod("get_datafile_rid")

get_datafile_rid.default <- function(x) NULL
