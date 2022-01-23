#' Récupère un attachment
#'
#' Permet de récupérer les données d'un attachment en utilisant soit son rid soit
#' son titre.
#'
#' Lève une exception si la recherche ne retourne plus ou moins que 1 attachment
#'
#' @param attachment un rid de attachment, un objet `dido_attachment()` ou `dido_job()`
#' @param title le titre d'un attachment
#' @param dataset l'identifiant d'un dataset ou un objet `dido_dataset()`
#'
#' @return un objet [dido_attachment()]
#'
#' @family attachment
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_attachment("rid", dataset = dataset)
#' get_attachment(title = "title", dataset = dataset)
#' }
get_attachment <- function(attachment = NULL, title = NULL, dataset) {
  if (is.null(attachment) && is.null(title)) {
    msg <- glue::glue("Vous devez préciser un des deux arguments `rid` ou `title`")
    rlang::abort("error_bad_argument", message = msg)
  }
  if (missing(dataset)) {
    msg <- glue::glue("Vous devez préciser l'argument `dataset`")
    rlang::abort("error_bad_argument", message = msg)
  }

  if (!is.null(attachment) && !is.null(title)) {
    msg <- glue::glue("`attachment` et `title` sont données, la recherchera est faite par `attachment`")
    rlang::warn(message = msg)
  }

  dataset_id <- if (!is.null(dataset)) get_dataset_id(dataset) else NULL

  if (!is.null(title)) {
    df <- get_attachments()
    result <- find_by_column(df, title, "title", c("id", "rid"))
    dataset_id <- result$id
    rid <- result$rid
  } else {
    rid <- get_attachment_rid(attachment)
    dataset_id <- get_attachment_id_by_rid(rid)
    rid <- get_attachment_rid(attachment)
  }

  url <- glue::glue("/datasets/{dataset_id}/attachments/{rid}")
  result <- dido_api(method = "GET", path = url)

  attr(result, "id") <- dataset_id

  new_dido_attachment(result)
}

get_attachment_id_by_rid <- function(rid) {
  find_by_column(data = get_attachments(), string = stringr::fixed(rid), col = "rid")
}
