#' Liste les datafiles
#'
#' Retourne la liste des datafiles ou si un objet dataset est passé en argument,
#' la liste des datafiles de ce dataset.
#'
#' @param dataset optionnel, un objet dataset
#'
#' @return un tibble avec les datafiles
#' @export
#'
#' @examples
#' get_datafiles()
get_datafiles <- function(dataset = NULL) {
  if (!is.null(dataset) && !is.dido_dataset(dataset)) {
    abort_bad_argument_type("dataset", c("get_dataset()", "create_dataset()"))
  }
  ds <- get_datasets()
  if (nrow(ds) == 0) {
    return(tibble())
  }
  if (!is.null(dataset)) ds <- filter(ds, .data$id == dataset$id)
  df <- dplyr::select(ds, .data$id, .data$datafiles)
  as_tibble(tidyr::unnest(df, .data$datafiles))
}
