default_columns <- list(
  REGION = list(type = "cog_region_{COG_YEAR}", description = "Code de la région"),
  DEPARTEMENT = list(type = "cog_departement_{COG_YEAR}", description = "Code du département"),
  COMMUNE = list(type = "cog_commune_{COG_YEAR}", description = "Code de la commune"),
  EPCI = list(type = "cog_epci_{COG_YEAR}", description = "Code de l'EPCI"),
  IRIS = list(type = "cog_iris_{COG_YEAR}", description = "Code de l'iris"),
  ANNEE = list(type = "annee", description = "Millésime des données"),
  MOIS = list(type = "mois", description = "Mois des données")
)

#' Génère les lignes d'entête du CSV augmenté utilisé par DiDo
#'
#' Génère un tibble avec les lignes d'entêtes du CSV augmenté.
#'
#'
#'
#' @param tbl le dataframe/tablespace à augmenter
#' @param params une liste nommée décrivant les caractéristiques des colonnes
#' @param locale la locale à utiliser
#' @param cog_year le millésime du COG utilisé si besoin. Par défaut prend l'année en cours
#'
#' @return un tibble avec les 4 lignes de description du csv augmenté
#' @export
#'
#' @details Certains noms de variable sont connus par didoscalim qui génère
#'   automatiquement le type et la description. La liste complète de ces
#'   variables et des types/descriptions associés est :
#'
#' | nom de la variable | type                  | description             |
#' |--------------------|-----------------------|-------------------------|
#' | REGION             |cog_region_AAAA        | Code de la région       |
#' | DEPARTEMENT        |cog_departement_AAAA   | Code du département     |
#' | COMMUNE            |cog_commune_AAAA       | Code de la commune      |
#' | EPCI               |cog_epci_AAAA          | Code de l'EPCI          |
#' | IRIS               |cog_iris_AAAA          | Code de l'IRIS          |
#' | ANNEE              | n/a                   | Millésimes des données  |
#' | MOIS               | n/a                   | mois des données        |
#'
#' L'année `AAAA` est l'année courante, vous pouvez la modifier en passant le paramètre `cog_year`
#'
#' @seealso En complément, vous pouvez lire : [la description d'un fichier
#'   csv
#'   augmenté](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/datalake/api/040-csvfile/),
#'    [la liste des entêtes
#'   utilisables](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/datalake/api/210-headers/)
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   OPERATEUR = list(description = "L'opérateur", type = "texte"),
#'   CONSO = list(description = "La consommation", unit = "Mwh")
#' )
#' dido_csv(my_tibble)
#' }
dido_csv <- function(tbl, params = list(),
                     locale = readr::default_locale(),
                     cog_year = format(Sys.time(), "%Y")) {
  desc <- description_row(tbl, params)
  type <- type_row(tbl, params, locale, cog_year)
  unit <- unit_row(type, params)
  name <- name_row(tbl)

  dplyr::bind_rows(desc, type, unit, name, tbl)
}

#' Enregistre le fichier CSV augmenté utilisé par DiDo.
#'
#' @param tbl un tibble retourné par `dido_csv()`
#' @param file le nom du fichier
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_dido_csv(my_tibble, "fichier.csv")
#' }
dido_write_csv <- function(tbl, file) {
  readr::write_delim(tbl,
    file,
    delim = ";",
    na = "",
    col_names = FALSE,
    quote = "all"
  )
}

#' Lit un fichier CSV
#'
#' Cette fonction utilise directement `readr::read_delim` en enlevant la
#' détection du type des colonnes.
#'
#' @inheritParams readr::read_delim
#'
#' @return un tibble dont toutes les colonnes sont de type `chr`
#'
#' @details Certaines variables peuvent avoir des valeurs secrétisées
#'   représentées par la valeur `secret`, la détection automatique de readr
#'   n'est donc pas fiable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- dido_read_delim("fichier.csv")
#' }
dido_read_delim <- function(file, delim = NULL, quote = '"',
                            escape_backslash = FALSE, escape_double = TRUE,
                            locale = readr::default_locale(),
                            comment = "", trim_ws = FALSE,
                            skip = 0, n_max = Inf,
                            skip_empty_rows = TRUE) {
  readr::read_delim(
    file = file,
    delim = delim,
    quote = quote,
    col_types = readr::cols(.default = "c"),
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    locale = locale,
    comment = comment,
    trim_ws = trim_ws,
    skip = skip, n_max = n_max,
    skip_empty_rows = skip_empty_rows
  )
}

#' @noRd
description_row <- function(tbl, params = list()) {
  name_cols <- vapply(names(tbl), function(name) {
    params[[name]][["description"]] %||%
      default_columns[[name]][["description"]] %||%
      name
  }, character(1))
  return(name_cols)
}

#' @noRd
unit_row <- function(tbl_type, params = list()) {
  tbl_unit <- vapply(names(tbl_type), function(name) {
    params[[name]][["unit"]] %||%
      if (grepl("(nombre|entier)", tbl_type[[name]])) "s/u" else "n/a"
  }, character(1))
}

list_types <- list(
  double = "nombre",
  integer = "entier",
  logical = "booleen",
  date = "jour"
)

#' @noRd
guess_col <- function(column, locale) {
  list_types[[guess_parser(column,
    na = c("", "na", "s", "secret"),
    guess_integer = TRUE,
    locale = locale
  )]]
}

#' @noRd
type_row <- function(tbl, params = list(), locale, cog_year) {
  cog_year <- toString(cog_year)

  guess_cols <- vapply(names(tbl), function(name) {
    type <- params[[name]][["type"]] %||%
      default_columns[[name]][["type"]] %||%
      guess_col(tbl[[name]], locale) %||%
      "texte"

    str_replace(type, "\\{COG_YEAR\\}", cog_year)
  }, character(1))
}

#' @noRd
name_row <- function(tbl) {
  name_cols <- vapply(names(tbl), function(name) {
    name
  }, character(1))
  return(name_cols)
}
