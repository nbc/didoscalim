#' Récupère un datafile
#'
#' Permet de récupérer les données d'un datafile en utilisant soit son rid soit
#' son titre.
#'
#' Lève une exception si la recherche ne retourne plus ou moins que 1 datafile
#'
#' @param datafile un rid de datafile, un objet `dido_datafile()` ou `dido_job()`
#' @param title le titre d'un datafile
#' @param dataset optionnel l'identifiant du dataset ou un objet `dido_dataset()`,
#'   fournir cet argument évite un appel à l'API
#'
#' @return un objet [dido_datafile()]
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_datafile("rid")
#' get_datafile(title = "title")
#' }
get_datafile <- function(datafile = NULL, title = NULL, dataset = NULL) {
  if (is.null(datafile) && is.null(title)) {
    msg <- glue::glue("Vous devez préciser un des deux arguments `rid` ou `title`")
    rlang::abort("error_bad_argument", message = msg)
  }

  if (!is.null(datafile) && !is.null(title)) {
    msg <- glue::glue("`datafile` et `title` sont données, la recherchera est faite par `datafile`")
    rlang::warn(message = msg)
  }

  dataset_id <- if (!is.null(dataset)) get_dataset_id(dataset) else NULL

  if (!is.null(title)) {
    df <- get_datafiles()
    result <- find_by_column(df, title, "title", c("id", "rid"))
    dataset_id <- result$id
    rid <- result$rid
  } else {
    rid <- get_datafile_rid(datafile)
    if (is.null(dataset_id)) dataset_id <- get_datafile_id_by_rid(rid)
    rid <- get_datafile_rid(datafile)
  }

  url <- glue::glue("/datasets/{dataset_id}/datafiles/{rid}")
  result <- dido_api(method = "GET", path = url)

  attr(result, "id") <- dataset_id

  new_dido_datafile(result)
}

get_datafile_id_by_rid <- function(rid) {
  find_by_column(data = get_datafiles(), string = stringr::fixed(rid), col = "rid")
}
