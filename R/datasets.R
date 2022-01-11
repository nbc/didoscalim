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

