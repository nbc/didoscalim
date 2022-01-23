#' Liste les attachments
#'
#' Retourne la liste des attachments ou si un objet dataset est passé en argument,
#' la liste des attachments de ce dataset.
#'
#' @param dataset optionnel, un objet dataset
#'
#' @return un tibble avec les attachments
#'
#' @family attachment
#'
#' @export
#'
#' @examples
#' get_attachments()
get_attachments <- function(dataset = NULL) {
  if (!is.null(dataset) && is.null(get_dataset_id(dataset))) {
    abort_not_dataset()
  }
  ds <- get_datasets()
  if (nrow(ds) == 0) {
    return(tibble())
  }
  if (!is.null(dataset)) ds <- filter(ds, .data$id == get_dataset_id(dataset))
  df <- dplyr::select(ds, .data$id, .data$attachments)
  as_tibble(tidyr::unnest(df, .data$attachments))
}
