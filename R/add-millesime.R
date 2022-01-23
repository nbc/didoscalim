#' Ajoute un millésime à un datafile
#'
#' @param datafile un objet dido_datafile obtenu par `get_datafile()`
#' @param file_name le fichier à charger
#' @param date_diffusion la date/heure de diffusion au format ISO 8601, si non
#'   précisée, le défaut est minuit du jour courant
#' @param millesime l'identifiant du millésime à publier, si non précisé AAAA-MM avec l'année et le mois courant
#' @param quiet quand TRUE ou que l'option dido_quiet est à TRUE supprime les
#'   messages d'information, `NULL` par défaut
#'
#' @return un objet `dido_job()`
#' @export
#'
#' @examples
#' \dontrun{
#' millesime <- add_millesime(
#'   datafile = datafile,
#'   file_name = "csv_upload.csv",
#'   millesime = "2022-10"
#' )
#' }
add_millesime <- function(datafile,
                          file_name,
                          date_diffusion = NULL,
                          millesime = NULL,
                          quiet = TRUE) {
  if (missing(datafile) || is.null(datafile)) abort_bad_argument("datafile")
  if (missing(file_name)) abort_bad_argument("file_name")
  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()


  token_file <- upload_file(file_name)
  check_csv(token_file)

  payload <- list(
    "tokenFile" = token_file
  )
  payload$date_diffusion <- date_diffusion %||% format(Sys.time(), "%Y-%m-%dT%H:00:00.000Z")
  payload$millesime <- millesime %||% format(Sys.time(), "%Y-%m")

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}")
  body <- jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")

  job <- dido_api(method = "POST", path = url, body = body)
  job_result <- dido_job(wait_for_job(job$id))

  if (!is_quiet(quiet)) {
    rlang::inform(message = glue::glue(
      "        * fichier intégré\n",
      "(rid: {job_result$result$rid}, ",
      "millesime: {job_result$result$millesime}, ",
      "lignes: {job_result$result$rows}"
    ))
  }

  job_result
}
