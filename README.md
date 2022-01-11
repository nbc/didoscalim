DiDo alimentation package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiDoscalim

<!-- badges: start -->

[<img src="https://www.repostatus.org/badges/latest/wip.svg" target="_blank" alt="Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public." />](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/MTES-MCT/didor/workflows/R-CMD-check/badge.svg)](https://github.com/MTES-MCT/didor/actions)
[![Codecov test
coverage](https://codecov.io/gh/nbc/didor/branch/main/graph/badge.svg)](https://codecov.io/gh/nbc/didor?branch=main)
<!-- badges: end -->

package R pour alimenter l’outil de diffusion de données DiDo du
[CGDD/SDES](https://www.statistiques.developpement-durable.gouv.fr/).

## Installation

Vous pouvez installer ce package depuis GitHub:

``` r
# Install from GitHub
library(devtools)
devtools::install_github("mtes-mct/didoscalim")
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

Pour vérifier que vos environnements sont bien configurés :

``` r
library(didoscalim)
readRenviron("~/.Renviron")
load_envs()
check_config()
```

Vous pouvez trouver l’ensemble de la documentation sur [la page du
projet](https://mtes-mct.github.io/didoscalim/).

## Tests

Les tests nécessitent un environnement de développement DiDo configuré.

DIDOSCALIM\_BASE\_PATH\_PROD=<http://api.alimentation.prod.fr/v1>
DIDOSCALIM\_API\_KEY\_PROD=ma\_clef\_api\_prod
