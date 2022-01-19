#' Récupère les métadonnées d'un dataset
#'
#' Permet de récupérer les données d'un dataset en utilisant soit son id soit
#' son titre.
#'
#' Lève une exception si la recherche ne retourne plus ou moins que 1 dataset
#' trouvé est différent de 1.
#'
#' @param id l'identifiant du dataset
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
get_dataset <- function(id = NULL, title = NULL) {
  if (is.null(id) && is.null(title)) {
    msg <- glue::glue("Vous devez préciser un des deux arguments `id` ou `title`")
    rlang::abort("error_bad_argument", message = msg)
  }
  if (!is.null(id) && !is.null(title)) {
    msg <- glue::glue("`id` ou `title` sont données, la recherchera est faite par `rid`")
    rlang::warn(message = msg)
  }

  if (!is.null(title)) id <- find_by_column(get_datasets(), title, "title")

  url <- glue::glue("/datasets/{id}")
  result <- dido_api(method = "GET", path = url)
  new_dido_dataset(result)
}
