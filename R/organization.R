#' Récupère l'ID de l'organisation de l'utilisateur.
#'
#' Si l'utilisateur appartient à plusieurs organisations
#'
#'
#'
#' @param name name or acronym for organization, default to NULL
#'
#' @return un id d'organisation
#' @export
#'
#' @examples
#' organization_id <- organization()
#' organization_id <- organization( name = "BSI" )
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
