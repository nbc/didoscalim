#' L'objet dido_datafile
#'
#' @description
#' L'objet dido_datafile et retourné par `get_datafile()` et peut-être utilisé
#' en paramètre par les fonctions de manipulation de dataset, datafile,
#' millésime et attachement.
#'
#' @name dido_datafile
NULL

#' Créé un objet dido_datafile
#'
#' Cette fonction est utilisée par `add_datafile()`
#'
#' @param dataset l'id d'un dataset, un objet `dido_dataset()`,
#'   `dido_datafile()` ou `dido_job()`
#' @param title le titre du datafile
#' @param description la description du datafile
#' @param millesime le millesime (YYYY-MM). Par défaut prendre la valeur YYYY-MM
#'   avec l'année courante et le mois courant
#' @param published la date de publication du fichier, si non précisée, prend la
#'   date du jour.
#' @param temporal_coverage_start la date de début des données au format
#'   YYYY-MM-DD
#' @param temporal_coverage_end la date de fin des données au format YYYY-MM-DD
#' @param legal_notice mention légale, par défaut "SDES"
#' @param date_diffusion la date/heure à laquelle le fichier sera accessible au
#'   format ISO 8601 (2021-10-01T08:00:00Z). Si non précisée prend la date du
#'   jour à minuit, le fichier est donc immédiatement accessible.
#'
#' @return un objet `[dido_datafile()]`
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
#' @keywords internal
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

#' @noRd
#' @export
extract_metadata.dido_datafile <- function(data) {
  data$millesimes_info <- NULL
  data
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
  if (!is.null(data$temporal_coverage$start)) data$temporal_coverage_start <- data$temporal_coverage$start
  if (!is.null(data$temporal_coverage$end)) data$temporal_coverage_end <- data$temporal_coverage$end

  data$temporal_coverage <- NULL
  data$millesimes_info <- NULL
  data$created_at <- NULL
  data$last_modified <- NULL
  data$millesimes <- NULL
  data$url <- NULL

  new_dido_datafile(data)
}
