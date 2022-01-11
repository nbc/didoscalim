#' Récupère la liste des datasets
#'
#' @return un tibble des datasets de l'utilisateur
#' @export
#'
#' @examples
#' get_datasets()
get_datasets <- function() {
  url <- "/datasets"
  dido_api(method = "GET", path = url, as_tibble = TRUE)
}

#' Récupère les datasets devant être mis à jour
#'
#' @return un tibble des datasets devant être mis à jour
#' @export
#'
#' @examples
#' get_alerts()
get_alerts <- function() {
  url <- "/datasets/alerts"
  dido_api(method = "GET", path = url, as_tibble = TRUE)
}

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
#' @return les métadonnées du dataset
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

#' @noRd
#' @examples
#' \dontrun{
#' get_datasets() %>%
#'   filter(str_detect(title, "didoscalim")) %>%
#'   select(id) %>%
#'   pmap(~ delete_dataset(..1))
#' }
delete_dataset <- function(id) {
  if (missing(id) || is.null(id)) abort_bad_argument("id")

  url <- glue::glue("/datasets/{id}")
  dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

#' @noRd
#' @export
print.dido_dataset_metadata <- function(x, ...) {
  str(x)
  invisible(x)
}
