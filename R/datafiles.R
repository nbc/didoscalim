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
