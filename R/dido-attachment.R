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
#' @family attachment
#'
#' @name dido_attachment
NULL

#' @noRd
#' @export
dido_attachment <- function(x) {
  structure(x, class = c("dido_attachment", "list"))
}

new_dido_attachment <- dido_attachment

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

#' @export
get_attachment_rid.dido_attachment <- function(data, ...) data$rid

#' @noRd
#' @export
clean_metadata.dido_attachment <- function(data) {
  new_dido_attachment(data)
}

#' @noRd
#' @export
internal_clean_metadata.dido_attachment <- function(data) {
  data$created_at <- NULL
  data$last_modified <- NULL
  data$last_update <- NULL
  data$rid <- NULL
  data$url <- NULL

  new_dido_dataset(data)
}
