#' Créé un dataset
#'
#' @param title le titre du dataset
#' @param description la description du dataset
#' @param topic le thème du dataset. Doit être dans "Environnement", "Énergie",
#'   "Transports", "Logement", "Changement climatique"
#' @param tags la liste des mots clefs. Si le fichier est déjà publié sur le
#'   site SDES, on reprend les mêmes. L'ensemble des mots clefs est disponible
#'   sur
#'   https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/referentiels/tags/csv?withColumnName=true&withColumnDescription=false.
#'
#' @param frequency la fréquence de publication des données
#' @param frequency_date la date de prochaine publication.
#' @param organization l'id de l'organisation sous laquelle vous souhaitez
#'   publier le dataset, si vous n'appartenez qu'à une seule organisation, vous
#'   n'avez pas à remplir ce champ, didoscalim la prendra par défaut.
#' @param temporal_coverage_start la date de début de couverture du jeux de
#'   données
#' @param temporal_coverage_end  la date de fin de couverture du jeux de données
#' @param zones la zone couverte par le jeu de données. country:fr pour France
#'   entière, country-subset:fr:metro pour la métropole et
#'   country-subset:fr:drom pour les DROM
#' @param granularity la granularité du jeu de données. Les plus utilisés sont
#'   "fr:region", "fr:departement", "fr:epci", "fr:commune", "fr:iris" et pour
#'   les données à l'adresse "poi"
#' @param caution Les précautions à prendre avec ce jeu de données. Par exemple
#'   : "certaines données peuvent être secrétisées"
#' @param license la licence des données. "fr-lo" par défaut
#' @param quiet quand TRUE ou que l'option dido_quiet est à TRUE supprime les
#'   messages d'information, `NULL` par défaut
#'
#' @return un objet [dido_dataset()]. Ce dernier sert pour créer par la suite
#'   les datafiles et les millésimes.
#'
#' @export
#'
#' @examples
#' dataset <- add_dataset(
#'   title = "le titre du dataset",
#'   description = "la description du dataset",
#'   topic = "Transports",
#'   frequency = "unknown"
#' )
add_dataset <- function(title,
                           description,
                           topic,
                           frequency,
                           tags = NULL,
                           frequency_date = NULL,
                           granularity = NULL,
                           zones = NULL,
                           organization = organization(),
                           license = "fr-lo",
                           temporal_coverage_start = NULL,
                           temporal_coverage_end = NULL,
                           caution = NULL,
                           quiet = NULL) {
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

  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("dataset `{title}` créé"))

  new_dido_dataset(ds)
}
