#' Créé un dataset
#'
#' @inheritParams dido_dataset
#' @param quiet Si TRUE n'affiche pas les messages. Défaut à `FALSE`
#'
#' @return un id de dataset. Ce dernier sert pour créer par la suite les
#'   datafiles et les millésimes.
#' @export
#'
#' @examples
#' dataset <- create_dataset(
#'   title = "test",
#'   description = "test",
#'   topic = "Transports",
#'   frequency = "unknown"
#' )
create_dataset <- function(title,
                           description,
                           topic,
                           frequency,
                           tags = NULL,
                           frequency_date = NULL,
                           granularity = NULL,
                           zones = NULL,
                           organization = NULL,
                           license = "fr-lo",
                           temporal_coverage_start = NULL,
                           temporal_coverage_end = NULL,
                           caution = NULL,
                           quiet = FALSE) {
  dataset <- dido_dataset(
    title = title,
    description = description,
    topic = topic,
    frequency = frequency,
    tags = tags,
    frequency_date = frequency_date,
    granularity = granularity,
    zones = zones,
    organization = organization,
    license = license,
    temporal_coverage_start = temporal_coverage_start,
    temporal_coverage_end = temporal_coverage_end,
    caution = caution
  )

  body <- jsonlite::toJSON(dataset, pretty = TRUE, auto_unbox = TRUE, na = "null")
  ds <- dido_api(method = "POST", path = "/datasets", body = body)

  if (!quiet) rlang::inform(message = glue::glue("dataset `{title}` créé"))

  new_dido_dataset(ds)
}
