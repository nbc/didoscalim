#' L'objet dido_datafile
#'
#' @description
#' L'objet dido_datafile et retourné par `get_datafile()` et peut-être utilisé
#' en paramètre par les fonctions de manipulation de dataset, datafile,
#' millésime et attachement.
#'
#' Il a les champs suivants :
#' * `rid` l'identifiant du datafile
#' * `title`
#' * `description`
#' * `millesimes` le nb de millésimes dans le datafile
#' * `created_at`
#' * `last_modified`
#' * `published`
#' * `url`
#' * `temporal_coverage` un liste avec deux champs `start` et `end`
#' * `legal_notice` la licence s'appliquant au fichier de données
#' * `millesimes_info` comprenant une liste des millésimes rattachés
#'
#' le dernier champ est supprimé par `clean_metadata()`
#'
#' L'objet a également un attribut `id` avec l'identifiant du dataset auquel il
#' appartient
#'
#' @family datafile
#'
#' @name dido_datafile
NULL

#' Créé un objet dido_datafile
#'
#' Cette fonction est utilisée par `add_datafile()`
#'
#' @inheritParams add_datafile
#'
#' @return un objet `dido_datafile()`
#'
#' @examples
#' \dontrun{
#' datafile <- dido_datafile(
#'   dataset = "1",
#'   title = "titre",
#'   description = "description",
#'   date_diffusion = "2022-01-01T08:00:00Z"
#' )
#' }
#' @noRd
dido_datafile <- function(dataset,
                          title,
                          description,
                          millesime = NULL,
                          published = NULL,
                          temporal_coverage_start = NULL,
                          temporal_coverage_end = NULL,
                          legal_notice = "SDES",
                          date_diffusion = NULL) {
  if (is.null(get_dataset_id(dataset))) abort_not_dataset()
  payload <- list(
    "title" = title,
    "description" = description,
    "millesime" = millesime
  )
  if (!is.null(temporal_coverage_start)) payload$temporal_coverage_start <- temporal_coverage_start
  if (!is.null(temporal_coverage_end)) payload$temporal_coverage_end <- temporal_coverage_end
  if (!is.null(legal_notice)) payload$legal_notice <- legal_notice

  payload$published <- published %||% format(Sys.time(), "%Y-%m-%d")
  payload$date_diffusion <- date_diffusion %||% format(Sys.time(), "%Y-%m-%dT00:00:00.000Z")
  payload$millesime <- millesime %||% format(Sys.time(), "%Y-%m")

  attr(payload, "id") <- get_dataset_id(dataset)

  new_dido_datafile(payload)
}

new_dido_datafile <- function(x) {
  structure(x, class = c("dido_datafile", "list"))
}

is.dido_datafile <- function(x) inherits(x, "dido_datafile")

#' @export
print.dido_datafile <- function(x, ...) {
  str(x)
  invisible(x)
}
#' @export
get_dataset_id.dido_datafile <- function(data, ...) attr(data, "id")

#' @export
get_datafile_rid.dido_datafile <- function(data, ...) data$rid

#' @noRd
#' @export
clean_metadata.dido_datafile <- function(data) {
  data$millesimes_info <- NULL

  new_dido_datafile(data)
}

#' @noRd
internal_clean_metadata.dido_datafile <- function(data) {
  if (!is.null(data$temporal_coverage$start)) data$temporal_coverage_start <- data$temporal_coverage$start
  if (!is.null(data$temporal_coverage$end)) data$temporal_coverage_end <- data$temporal_coverage$end

  data$rid <- NULL
  data$temporal_coverage <- NULL
  data$millesimes_info <- NULL
  data$created_at <- NULL
  data$last_modified <- NULL
  data$millesimes <- NULL
  data$url <- NULL

  new_dido_datafile(data)
}
