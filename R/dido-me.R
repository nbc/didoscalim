#' L'objet dido_me
#'
#' @description
#' L'objet dido_me est retourné par `me()`
#'
#' @name dido_me
#' @keywords internal
NULL

new_dido_me <- function(data) {
  structure(data, class = c("dido_me", "list"))
}

#' @export
print.dido_me <- function(x, ...) {
  str(x)
  invisible(x)
}
