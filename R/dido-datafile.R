#' L'objet dido_datafile
#'
#' @description
#' L'objet dido_datafile inclus les champs :
#' The response object captures all information from a request.  It includes
#' fields:
#'
#' * `url` the url the request was actually sent to (after redirects)
#' * `handle` the handle associated with the url
#' * `status_code` the http status code
#' * `header` a named list of headers returned by the server
#' * `cookies` a named list of cookies returned by the server
#' * `content` the body of the response, as raw vector. See [content()] for various ways to access the content.
#' * `time` request timing information
#' * `config` configuration for the request
#'
#' @details For non-http(s) responses, some parts including the status and
#'   header may not be interpretable the same way as http responses.
#'
#' @name dido_datafile
NULL

#' Créé un datafile
#'
#' @param dataset l'id d'un dataset ou un objet dataset tel que retourné par
#'   `get_dataset()` ou `create_dataset()`
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
#' @param date_diffusion date/heure de diffusion du fichier au format ISO 8601
#'   (2021-10-01T08:00:00Z). Si non précisée prend la date du jour à minuit.
#'
#' @return un objet job_result
#' @export
#'
#' @examples
#' \dontrun{
#' datafile <- create_datafile(
#'   dataset = dataset$id,
#'   title = "titre",
#'   description = "description",
#'   token_file = file_id
#' )
#' }
dido_datafile <- function(dataset,
                          title,
                          description,
                          millesime = NULL,
                          published = NULL,
                          temporal_coverage_start = NULL,
                          temporal_coverage_end = NULL,
                          legal_notice = "SDES",
                          date_diffusion = NULL) {
  if (!is.dido_dataset(dataset)) abort_bad_argument_type("dataset", c("get_dataset()", "create_dataset()"))

  payload <- list(
    "title" = title,
    "description" = description,
    "millesime" = millesime,
    "id" = get_id(dataset)
  )
  if (!is.null(temporal_coverage_start)) payload$temporal_coverage_start <- temporal_coverage_start
  if (!is.null(temporal_coverage_end)) payload$temporal_coverage_end <- temporal_coverage_end
  if (!is.null(legal_notice)) payload$legal_notice <- legal_notice

  payload$published <- published %||% format(Sys.time(), "%Y-%m-%d")
  payload$date_diffusion <- date_diffusion %||% format(Sys.time(), "%Y-%m-%dT00:00:00.000Z")
  payload$millesime <- millesime %||% format(Sys.time(), "%Y-%m")

  new_dido_datafile(payload)
}

new_dido_datafile <- function(x) {
  structure(x, class = c("dido_datafile", class(x)))
}

#' @noRd
#' @export
extract_metadata.dido_datafile <- function(data) {
  data$millesimes_info <- NULL
  data
}

is.dido_datafile <- function(x) inherits(x, "dido_datafile")

print.dido_datafile <- function(x, ...) {
  str(x)
  invisible(x)
}

get_id.dido_datafile <- function(x, ...) x$id

get_rid.dido_datafile <- function(x, ...) x$rid

#' @noRd
clean_metadata.dido_datafile <- function(df) {
  df <- unclass(df)
  if (!is.null(df$temporal_coverage$start)) df$temporal_coverage_start <- df$temporal_coverage$start
  if (!is.null(df$temporal_coverage$end)) df$temporal_coverage_end <- df$temporal_coverage$end

  df$id <- NULL
  df$rid <- NULL
  df$temporal_coverage <- NULL
  df$millesimes_info <- NULL
  df$created_at <- NULL
  df$last_modified <- NULL
  df$millesimes <- NULL
  # df$published <- NULL
  df$url <- NULL

  return(df)
}
