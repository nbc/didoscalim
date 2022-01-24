#' Récupère les datasets devant être mis à jour
#'
#' @return un tibble des datasets devant être mis à jour
#' @export
#'
#' @examples
#' get_alerts()
list_alerts <- function() {
  url <- "/datasets/alerts"
  dido_api(method = "GET", path = url, as_tibble = TRUE)
}
