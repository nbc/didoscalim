#' Liste les datafiles
#'
#' Retourne la liste des datafiles ou si un objet dataset est passé en argument,
#' la liste des datafiles de ce dataset.
#'
#' @param dataset optionnel, un objet dataset
#'
#' @return un tibble avec les datafiles
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' list_datafiles()
list_datafiles <- function(dataset = NULL) {
  if (!is.null(dataset) && is.null(get_dataset_id(dataset))) {
    abort_not_dataset()
  }
  ds <- list_datasets()
  if (nrow(ds) == 0) {
    return(tibble())
  }
  if (!is.null(dataset)) ds <- filter(ds, .data$id == get_dataset_id(dataset))
  df <- dplyr::select(ds, .data$id, .data$datafiles)
  as_tibble(tidyr::unnest(df, .data$datafiles))
}
