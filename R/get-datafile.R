#' Récupère un datafile
#'
#' Permet de récupérer les données d'un datafile en utilisant soit son rid soit
#' son titre.
#'
#' Lève une exception si la recherche ne retourne plus ou moins que 1 datafile
#'
#' @param rid l'identifiant du datafile
#' @param title l'identifiant du datafile
#' @param dataset optionnel l'identifiant du dataset ou un objet `dido_dataset`,
#'   fournir cet argument évite un appel à l'API
#'
#' @return un objet [dido_datafile()]
#' @export
#'
#' @examples
#' \dontrun{
#' get_datafile("rid")
#' get_datafile(title = "title")
#' }
get_datafile <- function(rid = NULL, title = NULL, dataset = NULL) {
  if (is.null(rid) && is.null(title)) {
    msg <- glue::glue("Vous devez préciser un des deux arguments `rid` ou `title`")
    rlang::abort("error_bad_argument", message = msg)
  }
  if (!is.null(rid) && !is.null(title)) {
    msg <- glue::glue("`rid` ou `title` sont données, la recherchera est faite par `rid`")
    rlang::warn(message = msg)
  }
  dataset_id <- if (!is.null(dataset)) get_dataset_id(dataset) else NULL
  if (!is.null(rid) && is.null(dataset_id)) dataset_id <- get_datafile_id_by_rid(rid)
  if (!is.null(title)) {
    df <- get_datafiles()
    result <- find_by_column(df, title, "title", c("id", "rid"))
    dataset_id <- result$id
    rid <- result$rid
  }
  url <- glue::glue("/datasets/{dataset_id}/datafiles/{rid}")
  result <- dido_api(method = "GET", path = url)
  result$id <- dataset_id
  new_dido_datafile(result)
}

get_datafile_id_by_rid <- function(rid) {
  find_by_column(data = get_datafiles(), string = stringr::fixed(rid), col = "rid")
}
