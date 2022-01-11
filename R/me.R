#' Get user's information
#'
#' @return user's informations
#' @export
#'
#' @examples
#' user <- me()
me <- function() {
  url <- "/users/me"
  result <- dido_api(method = "GET", path = url)
  structure(result, class = c("dido_me", "list"))
}

#' Get user's organization
#'
#' Return the user's organization. If user belong to multiple organization, you
#' can specify a name.
#'
#' @param name name or acronym for organization, default to NULL
#'
#' @return an organization Id
#' @export
#'
#' @examples
#' organization.id <- organization()
organization <- function(name = NULL) {
  orgs <- me()$organizations
  if (is.null(name)) {
    if (length(orgs) > 1) {
      rlang::abort("many_organizations",
        message = "Vous appartenez à plusieurs organisations, vous devez préciser celle à retenir en utilisant l'argument name"
      )
    }
    return(orgs[[1]]$id)
  }

  orgs <- orgs[grepl(name, orgs, ignore.case = TRUE)]
  if (length(orgs) > 1) {
    rlang::abort("multiple_organization", message = "La recherche donne plusieurs organisations, vous devez préciser votre recherche")
  } else if (length(orgs) < 1) {
    rlang::abort("bad_organization", message = "La recherche ne retourne aucune organisation")
  }

  return(orgs[[1]]$id)
}
