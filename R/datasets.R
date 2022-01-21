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

#' Supprime un dataset
#'
#' @param dataset un identifiant de dataset
#'
#' @return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' get_datasets() %>%
#'   filter(str_detect(title, "didoscalim")) %>%
#'   select(id) %>%
#'   pmap(~ delete_dataset(..1))
#' }
#' @export
#' @keywords internal
delete_dataset <- function(dataset) {
  if (missing(dataset)) abort_bad_argument("id")
  if (is.null(get_dataset_id(dataset))) abort_not_datafile()

  url <- glue::glue("/datasets/{get_dataset_id(dataset)}")
  dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}
