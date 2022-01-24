.didoscalim_env <- new.env()
ordered_env_names <- c("DEV", "PREPROD", "ECOLE")

#' @noRd
missing_key_message <- function(env_name, key) {
  glue::glue(
    "La configuration de l'environnement {env_name}",
    " est incomplète\nil manque la clef {key}_{env_name}",
    " dans le .Renviron"
  )
}

#' Recharge les environnements
#'
#' Cette commande permet de recharger les environnements si vous avez modifié le Renviron.
#' Vous devez d'abord le relire avec [readRenviron()]
#'
#' @export
#'
#' @examples
#' readRenviron("~/.Renviron")
#' load_envs()
load_envs <- function() {
  rm(list = ls(pos = .didoscalim_env), pos = .didoscalim_env)

  e <- Sys.getenv()
  dido_env <- e[grep("DIDOSCALIM_.*", names(e))]
  config <- list()
  for (n in names(dido_env)) {
    v <- stringr::str_split(n, "_")
    env <- v[[1]][4]
    name <- paste0(v[[1]][2:3], collapse = "_")

    config[[env]][[name]] <- dido_env[[n]]
  }
  for (env_name in names(config)) {
    if (is.null(config[[env_name]][["API_KEY"]])) {
      rlang::abort("config_error", message = missing_key_message(env_name, "DIDOSCALIM_API_KEY"))
    }
    if (is.null(config[[env_name]][["BASE_PATH"]])) {
      rlang::abort("config_error", message = missing_key_message(env_name, "DIDOSCALIM_BASE_PATH"))
    }
  }
  assign("environments", config, envir = .didoscalim_env)
}

#' Liste les environnements configurés
#'
#' Liste les environnements configurés avec les URL associées.
#'
#' @export
#'
#' @examples
#' list_envs()
list_envs <- function() {
  envs <- get("environments", envir = .didoscalim_env)
  message <- c(glue::glue("Vous avez {length(envs)} environnement(s) configuré(s) : "))
  for (e in names(envs)) {
    message <- c(message, i = glue::glue("{e} : {envs[[e]][['BASE_PATH']]}"))
  }
  cat(format_error_bullets(message))
}

#' @noRd
list_env_names <- function() {
  envs <- get("environments", envir = .didoscalim_env)
  names(envs)
}

#' Fixe l'environnement DiDo à utiliser
#'
#' @param env_name le nom de l'environnement à utiliser parmi PROD, ECOLE,
#'   PREPROD, DEV et suivant ce que vous avez configuré. Si env_name n'est pas
#'   passé, `set_work_env()` choisira le premier environnement configuré parmi
#'   DEV, PREPROD et ECOLE mais **jamais** PROD.
#' @param quiet quand TRUE ou que l'option dido_quiet est à TRUE supprime les
#'   messages d'information, `FALSE` par défaut
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_work_env("PROD")
#' }
set_work_env <- function(env_name = NULL, quiet = NULL) {
  environments <- get("environments", envir = .didoscalim_env)

  if (length(environments) == 0) {
    rlang::abort("env_error", message = "Aucun environnement n'est configuré dans votre Renviron")
  }

  if (is.null(env_name)) {
    env_name <- find_lowest_env()
    if (is.null(env_name)) {
      message <- c(
        glue::glue("Impossible de trouver l'environnement."),
        i = "Avez-vous configuré les environnements dans .Renviron ?"
      )
      rlang::abort("env_error", message = message)
    }
  } else if (!env_name %in% c(ordered_env_names, "PROD")) {
    message <- c(
      glue::glue("`{env_name}` n'est pas un environnement reconnu"),
      i = glue::glue("les environnements configurés dans Renviron sont {paste0(list_env_names(), collapse=", ")}")
    )
    rlang::abort("env_error", message = message)
  }
  if (!env_name %in% names(environments)) {
    rlang::abort("env_error", message = glue::glue("L'environnement {env_name} n'existe pas."))
  }

  if (!is_quiet(quiet)) rlang::inform(message = c(x = glue::glue("Environnement DiDo actif : {env_name}")))
  assign("work_env", env_name, envir = .didoscalim_env)
}

#' Récupère l'environnement utilisé
#'
#' Si aucun environnement n'a été fixé par `set_work_env()`, `get_work_env()`
#' retournera le premier environnement configuré en suivant l'ordre : DEV,
#' PREPROD, ECOLE. Il ne fixera **jamais** PROD implicitement.
#'
#' @inheritParams set_work_env
#'
#' @return une chaine de caractère avec l'environnement utilisé. Exemple "ECOLE"
#' @export
#'
#' @examples
#' get_work_env()
get_work_env <- function(quiet = NULL) {
  if (!exists("work_env", envir = .didoscalim_env)) set_work_env(quiet = quiet)
  get("work_env", envir = .didoscalim_env)
}

#' @noRd
api_key <- function() {
  environments <- get("environments", envir = .didoscalim_env)
  environments[[get_work_env()]][["API_KEY"]]
}

#' @noRd
base_path <- function() {
  environments <- get("environments", envir = .didoscalim_env)
  environments[[get_work_env()]][["BASE_PATH"]]
}

find_lowest_env <- function() {
  environments <- get("environments", envir = .didoscalim_env)
  for (n in ordered_env_names) {
    if (!is.null(environments[[n]])) {
      return(n)
    }
  }
}

#' Vérifier l'accès aux différents environnements
#'
#' Affiche un état des connexions aux différents environnements avec le message
#' d'erreur le cas échéant
#'
#' @export
#'
#' @examples
#' check_envs()
check_envs <- function() {
  old_env <- get_work_env(quiet = TRUE)
  message <- c("Test de connexion:")
  for (e in list_env_names()) {
    set_work_env(e, quiet = TRUE)
    tryCatch(
      {
        me()
        message <- c(message, i = glue::glue("{e}: OK"))
      },
      error = function(error) {
        message <<- c(message, x = glue::glue("{e}: KO: {stringr::str_replace(error$message, '\n.*', '')}"))
      }
    )
  }
  cat(format_error_bullets(message))
  set_work_env(old_env)
}
