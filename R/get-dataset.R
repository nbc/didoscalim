#' Récupère les métadonnées d'un dataset
#'
#' Permet de récupérer les données d'un dataset en utilisant soit son titre soit
#' un objet `dido_datafile()`, `dido_dataset()`, `dido_job()`
#'
#' Lève une exception si la recherche ne retourne pas exactement 1 dataset
#'
#' @param data l'identifiant d'un dataset ou un objet `dido_dataset()`, `dido_job()` ou `dido_datafile()`
#' @param title le titre du du dataset
#'
#' @return un objet [dido_dataset()]
#' @export
#'
#' @examples
#' \dontrun{
#' get_dataset(id)
#' get_dataset(id = id)
#' get_dataset(title = "un titre de dataset")
#' }
get_dataset <- function(data = NULL, title = NULL) {
  if (is.null(data) && is.null(title)) {
    msg <- glue::glue("Vous devez préciser un des deux arguments `data` ou `title`")
    rlang::abort("error_bad_argument", message = msg)
  }
  if (!is.null(data) && !is.null(title)) {
    msg <- glue::glue("`data` ou `title` sont données, la recherchera est faite par `data`")
    rlang::warn(message = msg)
  }

  id <- if (!is.null(title)) {
    find_by_column(get_datasets(), title, "title")
  } else {
    get_dataset_id(data)
  }

  url <- glue::glue("/datasets/{id}")
  result <- dido_api(method = "GET", path = url)
  new_dido_dataset(result)
}
