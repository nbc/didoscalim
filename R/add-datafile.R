#' Ajoute un datafile à un dataset
#'
#' @inheritParams dido_datafile
#' @param file_name le nom du fichier à charger
#' @param quiet quand TRUE ou que l'option dido_quiet est à TRUE supprime les
#'   messages d'information, `NULL` par défaut
#'
#' @return un objet [dido_job()]
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
