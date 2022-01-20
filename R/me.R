#' Récupère les informations de l'utilisateur
#'
#' @return Les informations utilisateurs
#' @export
#'
#' @examples
#' user <- me()
#' @keywords internal
me <- function() {
  url <- "/users/me"
  result <- dido_api(method = "GET", path = url)
  new_dido_me(result)
}

