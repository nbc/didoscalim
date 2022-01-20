#' @noRd
abort_bad_argument <- function(arg, class = NULL) {
  msg <- glue::glue("`{arg}` est obligatoire et ne peut être null")

  rlang::abort("error_bad_argument", message = msg)
}

#' @noRd
abort_bad_argument_type <- function(arg, fun) {
  message <- c(
    glue::glue("`{arg}` n'est pas du type attendu"),
    i = glue::glue("Avez-vous généré `{arg}` à partir d'une des fonctions: {paste0(fun, collapse = ', ')}")
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' @noRd
abort_not_dataset <- function() {
  message <- c(
    glue::glue("`dataset` n'est pas du type attendu"),
    i = glue::glue("`dataset` doit être soit un id de dataset soit la valeur retournée par par une des fonctions : `create_dataset`, `get_dataset`, `get_datafile`, `add_datafile`")
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' @noRd
abort_not_datafile <- function() {
  message <- c(
    glue::glue("`datafile` n'est pas du type attendu"),
    i = glue::glue("`datafile` doit être un rid de datafile ou la valeur retournée par une des fonctions `get_datafile`, `add_datafile`")
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' cherche dans un tibble
#'
#' @param data le dataframe/tibble dans lequel rechercher
#' @param string la chaine à chercher
#' @param col la colonne dans laquelle chercher
#' @param return_col a vecteur de colonnes à retourner
#'
#' @return un id
#'
#' @examples
#' \dontrun{
#' find_by_column(data, "un titre", "title")
#' find_by_column(data = data, string = "un titre", "title", c("id", "rid"))
#' find_by_column(data = data, string = stringr::fixed("rid"), "rid")
#' }
#' @keywords internal
find_by_column <- function(data, string, col, return = c("id")) {
  if (nrow(data) == 0) rlang::abort(glue::glue("no_data"), message = glue::glue("data est vide"))
  founded <- filter(data, stringr::str_detect(string = .data[[col]], pattern = string))

  if (nrow(founded) > 1) {
    message <- c(
      glue::glue("la recherche `{string}` retourne {nrow(founded)} lignes(s)"),
      i = glue::glue("Votre chaine de recherche est-celle trop ou pas assez précise ?")
    )
    rlang::abort("too_many_data", message = message)
    if (nrow(founded) == 0) {
      message <- c(
        glue::glue("la recherche `{string}` retourne {nrow(founded)} lignes(s)")
      )
      rlang::abort("no_data", message = message)
    }
  }

  return(founded[return])
}
