user_agent <- "didoscalim"

#' Envoie une requête au serveur DiDo
#'
#' Envoie la requête au serveur et retourne le résultat sous forme d'un objet ou un dataframe si as_dataframe est à TRUE
#'
#' @param method une des méthodes GET/POST/PUT/DELETE
#' @param path le chemin de l'api
#' @param body le body de la requête
#' @param query_params les paramètres de la requête
#' @param headers les entêtes de la requête
#' @param as_tibble TRUE/FALSE si TRUE retourne un tibble à la place d'un objet.
#'   Défaut à FALSE
#'
#' @return un objet json ou un dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' job <- dido_api(method = "POST", path = url, body = body)
#' alerts <- dido_api(method = "GET", path = "/datasets/alerts", as_tibble = TRUE)
#' }
#' @keywords internal
dido_api <- function(method, path, body, query_params = list(), headers = c(), as_tibble = FALSE) {
  url <- paste0(base_path(), path)
  ua <- httr::user_agent(user_agent)

  headers["x-api-key"] <- api_key()
  if (!"content-type" %in% headers) headers["content-type"] <- "application/json"

  if (method == "GET") {
    response <- httr::GET(url, httr::add_headers(headers), ua)
  } else if (method == "POST") {
    response <- httr::POST(url, query = query_params, body = body, httr::add_headers(headers), ua)
  } else if (method == "PUT") {
    response <- httr::PUT(url, query = query_params, body = body, httr::add_headers(headers), ua)
  } else if (method == "DELETE") {
    response <- httr::DELETE(url, query = query_params, httr::add_headers(headers), ua)
  } else {
    rlang::abort(glue::glue("unknown method: {method}"))
  }

  if (httr::status_code(response) >= 400) {
    rlang::abort("api_error", message = extract_error(response))
  }
  content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nzchar(content) == 0) {
    return(TRUE)
  }

  result <- jsonlite::fromJSON(content, simplifyVector = FALSE, simplifyDataFrame = as_tibble, flatten = as_tibble)
  if (as_tibble) {
    return(as_tibble(result))
  }
  result
}

#' @noRd
extract_error <- function(response) {
  if (httr::status_code(response) >= 400 && httr::status_code(response) < 500) {
    msg <- c(x = glue::glue("Client error : {httr::status_code(response)}"))
  } else if (httr::status_code(response) >= 500) {
    msg <- c(x = glue::glue("Server error : {httr::status_code(response)}"))
  }

  if (httr::http_type(response) == "application/json") {
    json_msg <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
    msg <- c(x = glue::glue("{json_msg$message}: {json_msg$code}"))

    if (!is.null(json_msg$errors)) {
      if (inherits(json_msg$errors, "data.frame")) {
        errors <- tidyr::unnest(json_msg$errors, .data$messages) %>% tidyr::unite("errors", .data$field:.data$messages, sep = ": ")
        for (e in errors[["errors"]]) msg <- c(msg, x = e)
      } else if (inherits(json_msg$errors, "list")) {
        for (e in json_msg$errors) msg <- c(msg, x = e)
      } else {
        msg <- c(msg, x = json_msg$errors)
      }
    }
  }
  c(msg, i = glue::glue("url: {response$url}\n"))
}
