DiDoscalim alimentation package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiDoscalim

<!-- badges: start -->

[<img src="https://www.repostatus.org/badges/latest/wip.svg" target="_blank" alt="Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public." />](https://www.repostatus.org/#wip)
<!-- badges: end -->

Un package R pour :

-   générer des fichiers CSV au format DiDo (CSV augmenté)
-   automatiser l’alimentation l’outil de diffusion de données DiDo du
    [CGDD/SDES](https://www.statistiques.developpement-durable.gouv.fr/).

Ce package est encore en cours de développement.

## Exemple

Comment générer un fichier CSV augmenté à partir d’un fichier [CSV
normal](vignettes/exemple.csv) et le charger dans un dataset.

``` r
library(didoscalim)
library(magrittr, quietly = TRUE)

temp_file <- tempfile(fileext = ".csv")

params = list(
  OPERATEUR = list(description = "Nom de l'opérateur"),
  FILIERE = list(description = "Filière"),
  CODE_CATEGORIE_CONSOMMATION = list(description = "Catégorie de la consommation"),
  CODE_SECTEUR_NAF2 = list(description = "Code NAF à 2 positions du secteur (NAF rev2 2008)", type = "naf_division"),
  CONSO = list(description = "Consommation (en MWh)", unit = "MWh")
)

dido_read_delim("vignettes/exemple.csv") %>%
  dido_csv(params = params, cog_year = "2019") %>%
  dido_write_csv(temp_file)
```

L’intégrer dans DiDo :

``` r
dataset <- add_dataset(
  title = "Un jeu de données fictif",
  description = "Un jeu de données énergie fictif",
  topic = "Transports",
  temporal_coverage_start = "2020-01-01",
  temporal_coverage_end = "2020-12-31",
  frequency = "annual",
  frequency_date = "2021-10-10"
)

add_datafile(
  dataset = dataset,
  file_name = temp_file,
  title = "Données de consommation fictive – gaz – année 2020",
  description = "Consommations annuelles et nombre de points de livraison de chaleur et froid, par secteur d'activité",
  temporal_coverage_start = "2020-01-01",
  temporal_coverage_end = "2020-12-31",
  millesime = "2021-10"
)
```

## Installation

Vous pouvez installer ce package depuis GitHub :

``` r
# Install from GitHub
library(devtools)
devtools::install_github("mtes-mct/didoscalim")

library(didoscalim)
```

## Configuration

-   [utiliser les environnements](articles/les-environnements.html)

La configuration de didoscalim se fait dans votre .Renviron. Pour
l’éditer vous pouvez utiliser `usethis::edit_r_environ()`. Ajouter les
lignes suivantes (la clef d’API est disponible dans l’interface web
d’alimentation dans l’onglet “Mon compte”) :

Il est fortement recommandé de tester les chargements avant de les
lancer sur `PROD`, pour cela configurez au minimum les environnements
`ECOLE` et `PROD`.

Pour vérifier que vos environnements sont bien configurés :

``` r
library(didoscalim)
readRenviron("~/.Renviron")
load_envs()
check_envs()
```

Par défaut, didoscalim utilisera l’environnement `ECOLE`. pour basculer
sur l’environnement `PROD` vous pouvez lancer la commande :

``` r
set_work_env("PROD")
```

Vous pouvez à tout moment voir sur quel environnement vous travailler
avec la commande `get_work_env()`

Pour rebasculer sur l’environnement `ECOLE` :

``` r
set_work_env()
```

## Documentation

Vous pouvez trouver l’ensemble de la documentation sur [la page du
projet](https://mtes-mct.github.io/didoscalim/) et en particuler :

-   [comment générer un fichier augmenté](articles/csv-augmente.html)
-   [comment intégrer des nouveaux jeux/fichiers de
    données](articles/charger-des-donnees.html)
-   [utiliser les environnements](articles/les-environnements.html)

## Pour les développeurs

### Les tests

Les tests de ce package nécessitent pour le moment un environnement de
développement DiDo configuré pour tester les interactions avec l’API.

### Documentation en ligne

La documentation ne peut pas (encore ?) être généré par les github
actions, il faut donc pour le moment générer le site statiquement à
partir d’une poste qui a accès à un environnement DiDo de développement.

Si vous avez modifié la documentation, avant de pousser une nouvelle
vous devez lancer les commandes suivantes :

``` r
devtools::build_readme()
devtools::document()
devtools::build_vignettes()
devtools::build_site()
```
