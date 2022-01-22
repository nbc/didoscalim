#' L'objet dido_attachment
#'
#' @description
#' L'objet dido_attachment inclus les champs :
#' * `rid` l'identifiant du jeu de données parent
#' * `title`
#' * `description`
#' * `created_at`
#' * `last_modified`
#' * `published`
#' * `url` : l'url du fichier annexe
#'
#' @name dido_attachment
NULL

#' @noRd
#' @export
dido_attachment <- function(x) {
  structure(x, class = c("dido_attachment", "list"))
}

is.dido_attachment <- function(x) {
  inherits(x, "dido_attachment")
}

#' @noRd
#' @export
print.dido_attachment <- function(x, ...) {
  str(x)
  invisible(x)
}

#' @export
get_dataset_id.dido_attachment <- function(data, ...) attr(data, "id")
