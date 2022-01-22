#' L'objet dido_dataset
#'
#' @description
#' L'objet dido_dataset est retourné par `get_dataset()` et `add_dataset()`
#' et utilisé en paramètre par `update_dataset()` `add_datafile()` et
#' `create_attachment()`
#'
#' Il a les champs suivants :
#' * `id` l'identifiant du dataset
#' * `title`
#' * `description`
#' * `organization`
#' * `topic` le thème
#' * `license`
#' * `frequency` la fréquence
#' * `frequency_date` prochaine mise à jour
#' * `tags` une liste de mots clefs
#' * `spatial` une liste avec des champs, la `granularity` et `zones` une liste de zones
#' * `temporal_coverage` un liste avec deux champs `start` et `end`
#' * `created_at` la date de création
#' * `last_modified`
#' * `last_update`
#' * `attachments` une liste des attachments
#' * `datafiles` une liste des fichiers de données
#'
#' les deux derniers champs sont supprimés par `clean_metadata()`
#'
#' @name dido_dataset
NULL

#' Créé un objet dataset
#'
#' @inheritParams dido_dataset
#' @return un objet dido_dataset
#' @export
#'
#' @examples
#' dataset <- dido_dataset(
#'   title = "test",
#'   description = "test",
#'   topic = "Transports",
#'   frequency = "unknown"
#' )
#' @noRd
dido_dataset <- function(title,
                         description,
                         topic,
                         frequency,
                         tags = NULL,
                         frequency_date = NULL,
                         granularity = NULL,
                         zones = NULL,
                         organization = NULL,
                         license = "fr-lo",
                         temporal_coverage_start = NULL,
                         temporal_coverage_end = NULL,
                         caution = NULL) {
  payload <- list(
    "title" = title,
    "description" = description,
    "organization" = organization,
    "frequency" = frequency,
    "topic" = topic,
    "license" = license
  )
  payload$organization <- if (is.null(organization)) organization() else organization
  if (!is.null(frequency_date)) payload$frequency_date <- frequency_date
  if (!is.null(zones)) payload$spatial$zones <- zones
  if (!is.null(granularity)) payload$spatial$granularity <- granularity
  if (!is.null(tags)) payload$tags <- tags
  if (!is.null(caution)) payload$caution <- caution

  #  if (is.null(temporal_coverage_start) || is.null(temporal_coverage_end)) {
  #    stop("Les champs temporal_coverage_start temporal_coverage_end doivent être soit définis tous les deux, soit non définis tous les deux")
  #  }

  if (!is.null(temporal_coverage_start) && !is.null(temporal_coverage_end)) {
    payload$temporal_coverage <- list(
      start = temporal_coverage_start,
      end = temporal_coverage_end
    )
  }
  new_dido_dataset(payload)
}

new_dido_dataset <- function(x) {
  structure(x, class = c("dido_dataset", "list"))
}

#' @export
get_dataset_id.dido_dataset <- function(data, ...) data$id

is.dido_dataset <- function(x) inherits(x, "dido_dataset")

#' @export
print.dido_dataset <- function(x, ...) {
  str(x)
  invisible(x)
}

#' @noRd
#' @export
clean_metadata.dido_dataset <- function(data) {
  data$attachments <- NULL
  data$datafiles <- NULL

  new_dido_dataset(data)
}

#' @noRd
#' @export
internal_clean_metadata.dido_dataset <- function(data) {
  data$created_at <- NULL
  data$last_modified <- NULL
  data$last_update <- NULL
  data$attachments <- NULL
  data$datafiles <- NULL
  data$id <- NULL

  if ("id" %in% names(data$organization)) data$organization <- data$organization$id

  new_dido_dataset(data)
}
