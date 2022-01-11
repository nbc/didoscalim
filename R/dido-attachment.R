#' L'objet dido_attachment
#'
#' @description
#' L'objet dido_attachment inclus les champs :
#' The response object captures all information from a request.  It includes
#' fields:
#'
#' @name dido_attachment
NULL

#' @noRd
#' @export
dido_attachment <- function(x) {
  structure(x, class = c("dido_attachment", class(x)))
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
