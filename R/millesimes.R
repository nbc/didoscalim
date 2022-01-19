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
  if (!is.null(datafile) && is.null(get_datafile_rid(datafile))) abort_not_datafile()

  df <- get_datafiles()

  if (!is.null(datafile)) df <- filter(df, .data$rid == get_datafile_rid(datafile))

  ml <- dplyr::select(df, .data$id, .data$rid, .data$millesimes_info)
  as_tibble(tidyr::unnest(ml, .data$millesimes_info))
}
