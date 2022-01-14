#' Liste les datafiles
#'
#' @return un tibble avec les datafiles
#' @export
#'
#' @examples
#' get_datafiles()
get_datafiles <- function() {
  ds <- get_datasets()
  if (nrow(ds) == 0) {
    return(tibble())
  }
  df <- dplyr::select(ds, .data$id, .data$datafiles)
  as_tibble(tidyr::unnest(df, .data$datafiles))
}

#' Récupère un datafile
#'
#' Permet de récupérer les données d'un datafile en utilisant soit son rid soit
#' son titre.
#'
#' Lève une exception si la recherche ne retourne plus ou moins que 1 datafile
#'
#' @param rid l'identifiant du datafile
#' @param title l'identifiant du datafile
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_datafile("rid")
#' get_datafile(rid = "rid", id = "id")
#' get_datafile(title = "title")
#' }
get_datafile <- function(rid = NULL, title = NULL, id = NULL) {
  if (is.null(rid) && is.null(title)) {
    msg <- glue::glue("Vous devez préciser un des deux arguments `rid` ou `title`")
    rlang::abort("error_bad_argument", message = msg)
  }
  if (!is.null(rid) && !is.null(title)) {
    msg <- glue::glue("`rid` ou `title` sont données, la recherchera est faite par `rid`")
    rlang::warn(message = msg)
  }

  df <- get_datafiles()
  if (!is.null(rid) && is.null(id)) id <- find_by_column(df, stringr::fixed(rid), "rid")
  if (!is.null(title)) {
    result <- find_by_column(df, title, "title", c("id", "rid"))
    id <- result$id
    rid <- result$rid
  }

  url <- glue::glue("/datasets/{id}/datafiles/{rid}")
  result <- dido_api(method = "GET", path = url)
  structure(result, class = c("dido_datafile", class(result)))
}

#' Mettre à jour un datafile
#'
#' @param id l'identifiant du dataset
#' @param rid l'identifiant du datafile
#' @param metadata l'objet retourné par `get_datafile()`. Vous pouvez le "nettoyer" avec la fonction `clean_datafile_metadata()`
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' update_datafile("id", "rid", metadata)
#' }
update_datafile <- function(id, rid, metadata) {
  if (missing(id) || is.null(id)) abort_bad_argument("id")
  if (missing(rid) || is.null(rid)) abort_bad_argument("rid")
  if (missing(metadata)) abort_bad_argument("metadata")

  if (!any(class(metadata) %in% c("dido_datafile", "dido_datafile_metadata"))) {
    abort_bad_type_argument("metadata", c("get_datafile", "clean_datafile_metadata"))
  }

  if (!any(class(metadata) %in% c("dido_datafile_metadata"))) {
    metadata <- clean_datafile_metadata(metadata)
  }

  if (is.null(metadata$published)) metadata$published <- format(Sys.time(), "%Y-%m-%d")

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/metadata")
  body <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")

  result <- dido_api(method = "PUT", path = url, body = body)
  structure(result, class = c("dido_datafile", class(result)))
}


#' Nettoie l'objet retourné par `get_datafile()`
#'
#' @param df l'objet retourné par `get_datafile()`
#'
#' @return un objet `datafile` que l'on peut passer à `update_datafile()`
#' @export
#'
#' @examples
#' \dontrun{
#' df <- get_datafile("id", "rid")
#' df$temporal_coverage$end <- "2023-12-31"
#' update_datafile("id", "rid", df)
#' }
clean_datafile_metadata <- function(df) {
  df <- unclass(df)
  if (!is.null(df$temporal_coverage$start)) df$temporal_coverage_start <- df$temporal_coverage$start
  if (!is.null(df$temporal_coverage$end)) df$temporal_coverage_end <- df$temporal_coverage$end

  df$rid <- NULL
  df$temporal_coverage <- NULL
  df$millesimes_info <- NULL
  df$created_at <- NULL
  df$last_modified <- NULL
  df$millesimes <- NULL
  df$published <- NULL
  df$url <- NULL

  structure(df, class = c("dido_datafile_metadata", class(df)))
}
