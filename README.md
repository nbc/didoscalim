DiDoscalim alimentation package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiDoscalim

<!-- badges: start -->

[<img src="https://www.repostatus.org/badges/latest/wip.svg" target="_blank" alt="Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public." />](https://www.repostatus.org/#wip)
<!-- badges: end -->

Un package R pour générer des fichiers CSV au format DiDo (CSV augmenté)
et alimenter l’outil de diffusion de données DiDo du
[CGDD/SDES](https://www.statistiques.developpement-durable.gouv.fr/).

Ce package est encore en cours de développement.

## Installation

Vous pouvez installer ce package depuis GitHub :

``` r
# Install from GitHub
library(devtools)
devtools::install_github("nbc/didoscalim")

library(didoscalim)
```

## Configuration

La configuration de didoscalim se fait dans votre .Renviron. Pour
l’éditer vous pouvez utiliser `usethis::edit_r_environ()`. Ajouter les
lignes suivantes (la clef d’API est disponible dans l’interface web
d’alimentation dans l’onglet “Mon compte”) :

    DIDOSCALIM_BASE_PATH_ECOLE=http://api.alimentation.ecole.fr/v1
    DIDOSCALIM_API_KEY_ECOLE=ma_clef_api_ecole

    DIDOSCALIM_BASE_PATH_PROD=http://api.alimentation.prod.fr/v1
    DIDOSCALIM_API_KEY_PROD=ma_clef_api_prod

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
projet](https://nbc.github.io/didoscalim/) et en particuler :

-   [comment générer un fichier
    augmenté](https://nbc.github.io/didoscalim/articles/csv-augmente.html)
-   [comment intégrer des nouveaux jeux/fichiers de
    données](https://nbc.github.io/didoscalim/articles/premier-chargement.html)
-   [comment mettre à jour des jeux/fichiers de
    données](https://nbc.github.io/didoscalim/articles/mettre_a_jour_des_donnees.html)
    (en cours de rédaction)

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
