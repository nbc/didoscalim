#' Créé un datafile
#'
#' @inheritParams dido_datafile
#' @param quiet Si TRUE n'affiche pas les messages. Défaut à `FALSE`
#' @param file_name le nom du fichier à charger
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
create_datafile <- function(dataset,
                            title,
                            description,
                            file_name,
                            millesime = NULL,
                            published = NULL,
                            temporal_coverage_start = NULL,
                            temporal_coverage_end = NULL,
                            legal_notice = "SDES",
                            date_diffusion = NULL,
                            quiet = FALSE) {
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


  if (!quiet) rlang::inform(message = glue::glue("    intégration du fichier `{file_name}`"))
  datafile$tokenFile <- upload_file(file_name)
  if (!quiet) rlang::inform(message = glue::glue("\t* fichier versé"))
  check_csv(datafile$tokenFile)
  if (!quiet) rlang::inform(message = glue::glue("\t* fichier validé"))

  body <- jsonlite::toJSON(clean_metadata(datafile), pretty = TRUE, auto_unbox = TRUE, na = "null")

  id <- get_id(dataset)

  url <- glue::glue("/datasets/{id}/datafiles")
  job <- dido_api(method = "POST", path = url, body = body)
  job_result <- dido_job(wait_for_job(job$id))

  if (!quiet) {
    rlang::inform(glue::glue(
      "\t* fichier intégré",
      "\t    rid: {get_rid(job_result)}",
      "\t    millesime: {job_result$result$millesime}",
      "\t    lignes: {job_result$result$rows}"
    ))
  }
  job_result
}
