#' Remplacer un millésime
#'
#' @inheritParams add_millesime
#'
#' @return un job
#' @export
#'
#' @examples
#' \dontrun{
#' datafile <- get_datafile("rid")
#' millesime <- replace_millesime(
#'   datafile = datafile,
#'   file_name = "csv_upload.csv",
#'   millesime = "2022-10"
#' )
#' }
replace_millesime <- function(datafile,
                              file_name,
                              millesime,
                              date_diffusion = NULL,
                              quiet = FALSE) {
  if (missing(datafile) || is.null(datafile)) abort_bad_argument("datafile")
  if (!is.dido_datafile(datafile)) abort_bad_argument_type("datafile", c("get_datafile()", "extract_metadata()"))

  if (missing(millesime) || is.null(millesime)) abort_bad_argument("millesime")
  if (missing(file_name) || is.null(file_name)) abort_bad_argument("file_name")

  if (!quiet) rlang::inform(message = glue::glue("    intégration du fichier `{file_name}`"))
  token_file <- upload_file(file_name)
  if (!quiet) rlang::inform(message = glue::glue("\t* fichier versé"))
  check_csv(token_file)
  if (!quiet) rlang::inform(message = glue::glue("\t* fichier validé"))


  payload <- list(
    "tokenFile" = token_file
  )
  payload$date_diffusion <- date_diffusion %||% format(Sys.time(), "%Y-%m-%dT%H:00:00.000Z")

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/millesimes/{millesime}")
  body <- jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")

  job <- dido_api(method = "PUT", path = url, body = body)
  job_result <- dido_job(wait_for_job(job$id))

  if (!quiet) {
    rlang::inform(message = glue::glue(
      "        * fichier intégré\n",
      "(rid: {job_result$result$rid}, ",
      "millesime: {job_result$result$millesime}, ",
      "lignes: {job_result$result$rows}"
    ))
  }

  job_result
}
