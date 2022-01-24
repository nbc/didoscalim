#' Récupère tous les jobs liés à un utilisateur
#'
#' @return un tibble de jobs
#' @export
#'
#' @examples
#' jobs <- list_jobs()
list_jobs <- function() {
  url <- "/jobs"
  dido_api(method = "GET", path = url, as_tibble = TRUE)
}

#' Récupére un job en particulier
#'
#' @param job_id l'id du job
#'
#' @return un objet `dido_job()``
#' @export
#'
#' @examples
#' \dontrun{
#' job <- get_job("1")
#' }
get_job <- function(job_id) {
  url <- glue::glue("/jobs/{job_id}")
  job <- dido_api(method = "GET", path = url)
  dido_job(job)
}

#' Attend la fin d'un job d'intégration
#'
#' @param job_id l'id du job
#'
#' @return les données du job
#' @export
#'
#' @examples
#' \dontrun{
#' wait_for_job("id")
#' }
#' @keywords internal
wait_for_job <- function(job_id, quiet = NULL) {
  pb <- progress::progress_bar$new(
    total = 100,
    format = "chargement [:bar] :percent eta: :eta",
  )
  pb$tick(0)

  repeat {
    job <- get_job(job_id)
    if (!is.null(job$result)) {
      pb$terminate()
      return(dido_job(job))
    } else if (!is.null(job$error)) {
      pb$terminate()
      message <- c(glue::glue("   erreur {job$error$message}"))
      for (error in job$error$list) {
        message <- c(
          message,
          x = glue::glue("      ligne: {error$line} colonne: {error$column} {error$message}")
        )
      }

      rlang::abort("datafile_error", message = message)
    }
    if (job$state$status != "failed" && !is.null(job$state$progress$percentage)) {
      ratio <- strtoi(job$state$progress$percentage) / 100
      pb$update(ratio)
    }
    Sys.sleep(5)
  }
}
