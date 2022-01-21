#' Met à jour les metadonnées d'un dataset
#'
#' Cette fonction permet de mettre à jour les métadonnées d'un dataset comme,
#' par exemple, la date de fin de couverture temporelle.
#'
#' @param dataset un objet dido_dataset modifié
#'
#' @return un objet dataset
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- get_dataset("id") %>% clean_metadata()
#' dataset$temporal_coverage$end <- "2022-12-31"
#' update_dataset(dataset)
#' }
update_dataset <- function(dataset) {
  if (missing(dataset) || is.null(dataset)) abort_bad_argument("dataset")
  if (!is.dido_dataset(dataset)) abort_not_dataset()

  id <- dataset$id
  url <- glue::glue("/datasets/{id}")

  dataset$id <- NULL
  metadata <- clean_metadata(dataset)

  body <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")
  response <- dido_api(method = "PUT", path = url, body = body)
  invisible(new_dido_dataset(response))
}
