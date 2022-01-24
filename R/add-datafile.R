#' Ajoute un datafile à un dataset
#'
#' @param dataset l'id d'un dataset, un objet `dido_dataset()`,
#'   `dido_datafile()` ou `dido_job()`
#' @param title le titre du datafile
#' @param description la description du datafile
#' @param millesime le millesime (AAAA-MM). Par défaut prendre la valeur AAAA-MM
#'   avec l'année courante et le mois courant
#' @param published la date de publication du fichier, si non précisée, prend la
#'   date du jour.
#' @param temporal_coverage_start optionnel, la date de début de couverture du
#'   fichier de données au format AAAA-MM-JJ
#' @param temporal_coverage_end optionnel, la date de fin de couverture du
#'   fichier de données au format AAAA-MM-JJ
#' @param legal_notice les mentions légales, par défaut "SDES"
#' @param date_diffusion la date/heure à laquelle le fichier sera accessible. Si
#'   cette date est dans le passé, les données sont immédiatement accessibles,
#'   si elle est dans le futur, les données ne seront accessibles qu'à cette
#'   date/heure. au format ISO 8601 (2021-10-01T08:00:00Z) Si non précisée prend
#'   la date du au jour à minuit, le fichier est donc immédiatement accessible.
#' @param file_name le nom du fichier à charger
#' @param quiet quand `TRUE` ou que l'option dido_quiet est à `TRUE` supprime
#'   les messages d'information, `NULL` par défaut
#'
#' @return un objet `dido_job()`
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datafile <- add_datafile(
#'   dataset = dataset$id,
#'   title = "titre",
#'   description = "description",
#'   token_file = file_id
#' )
#' }
add_datafile <- function(dataset,
                         title,
                         description,
                         file_name,
                         millesime = NULL,
                         published = NULL,
                         temporal_coverage_start = NULL,
                         temporal_coverage_end = NULL,
                         legal_notice = "SDES",
                         date_diffusion = NULL,
                         quiet = NULL) {
  datafile <- dido_datafile(
    dataset = dataset,
    title = title,
    description = description,
    millesime = millesime,
    published = published,
    temporal_coverage_start = temporal_coverage_start,
    temporal_coverage_end = temporal_coverage_end,
    legal_notice = legal_notice,
    date_diffusion = date_diffusion
  )

  if (missing(file_name) || is.null(file_name)) abort_bad_argument("file_name")

  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("    intégration du fichier `{file_name}`"))
  datafile$tokenFile <- upload_file(file_name)
  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("\t* fichier versé"))
  check_csv(datafile$tokenFile)
  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("\t* fichier validé"))

  df <- clean_metadata(datafile)

  body <- jsonlite::toJSON(df, pretty = TRUE, auto_unbox = TRUE, na = "null")

  id <- get_dataset_id(dataset)

  url <- glue::glue("/datasets/{id}/datafiles")
  job <- dido_api(method = "POST", path = url, body = body)
  job_result <- dido_job(wait_for_job(job$id))

  if (!is_quiet(quiet)) {
    rlang::inform(glue::glue(
      "\t* fichier intégré",
      "\t    rid: {get_datafile_rid(job_result)}",
      "\t    millesime: {job_result$result$millesime}",
      "\t    lignes: {job_result$result$rows}"
    ))
  }
  invisible(job_result)
}
