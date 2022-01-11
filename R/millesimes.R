#' Liste les millesimes
#'
#' @param datafile un objet datafile, optionnel
#'
#' @return un tibble avec les millesimes du datafile
#' @export
#'
#' @examples
#' get_millesimes()
get_millesimes <- function(datafile = NULL) {
  if (!is.null(datafile) && !is.dido_datafile(datafile)) {
    abort_bad_argument_type("dataset", c("get_datafile()"))
  }

  df <- get_datafiles()

  if (!is.null(datafile)) df <- filter(df, .data$rid == datafile$rid)

  ml <- dplyr::select(df, .data$id, .data$rid, .data$millesimes_info)
  as_tibble(tidyr::unnest(ml, .data$millesimes_info))
}
