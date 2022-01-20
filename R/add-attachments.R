#' Ajoute un fichier annexe à un dataset
#'
#' @param dataset l'id d'un dataset de rattachement, un objet `dido_dataset()`,
#'   `dido_datafile()` ou `dido_job()`
#' @param title le titre du fichier annexe
#' @param description la description du fichier annexe
#' @param file_name le nom du fichier à verser
#' @param published la date de publication, si non précisée, prendra la date du jour
#' @param quiet quand TRUE supprime les messages d'information, `FALSE` par défaut
#'
#' @return un objet décrivant la pièce annexe
#' @export
#'
#' @examples
#' \dontrun{
#' add_attachment(
#'   dataset = "id",
#'   title = "title",
#'   description = "description",
#'   file_name = "filename"
#' )
#' }
add_attachment <- function(dataset,
                           title,
                           description,
                           file_name,
                           published = NULL,
                           quiet = FALSE) {
  if (missing(dataset) || is.null(dataset)) abort_bad_argument("dataset")
  if (is.null(get_dataset_id(dataset))) abort_not_dataset()

  if (missing(title) || is.null(title)) abort_bad_argument("title")
  if (missing(description) || is.null(description)) abort_bad_argument("description")
  if (missing(file_name) || is.null(file_name)) abort_bad_argument("file_name")

  if (!quiet) rlang::inform(message = glue::glue("    intégration du fichier annexe `{file_name}`"))

  file_id <- upload_file(file_name)
  if (!quiet) rlang::inform(message = glue::glue("\t* fichier versé"))

  payload <- list(
    "title" = title,
    "description" = description,
    "tokenFile" = file_id
  )
  payload$published <- if (!is.null(published)) published else format(Sys.time(), "%Y-%m-%d")

  id <- get_dataset_id(dataset)

  url <- glue::glue("/datasets/{id}/attachments")
  result <- dido_api(
    method = "POST",
    path = url,
    body = jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")
  )
  if (!quiet) rlang::inform(glue::glue("\t* fichier annexe intégré (rid: {result$rid})"))
  attr(result, "id") <- id

  invisible(dido_attachment(result))
}
