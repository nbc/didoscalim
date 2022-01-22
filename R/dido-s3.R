#' Prépare un objet pour l'envoyer à DiDo
#'
#' Les objets `dido_dataset()` et `dido_datafile()` portent des informations sur
#' les objets enfants (fichiers annexes, datafile, millésime...). Cette fonction
#' permet de les supprimer pour ne garder que les métadonnées propres à l'objet.
#'
#' @param data objet dido_dataset, dido_datafile
#'
#' @return un objet du même type avec uniquement ses métadonnées propres.
#' @export
#'
#' @examples
#' \dontrun{
#' clean_metadata(dataset)
#' }
clean_metadata <- function(data) UseMethod("clean_metadata")

#' @noRd
internal_clean_metadata <- function(data) UseMethod("internal_clean_metadata")

#' Retourne le dataset id de l'objet
#'
#' @param data un objet dido_dataset, dido_datafile ou dido_job
#'
#' @return le dataset id
#' @export
#'
#' @examples
#' \dontrun{
#' get_dataset_id(dataset)
#' }
#' @keywords internal
get_dataset_id <- function(data) UseMethod("get_dataset_id")

#' @export
get_dataset_id.default <- function(data) NULL

#' @export
get_dataset_id.character <- function(data) data

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

#' @export
get_datafile_rid.default <- function(data) NULL

#' @export
get_datafile_rid.character <- function(data) data
