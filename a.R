dataset <- get_dataset(title = "bbbbbbb")

create_datafile(
  dataset = dataset,
  file_name = "test.csv",
  title = "AAAAAAAA Données de consommation et de points de livraison d'énergie à la maille IRIS – chaleur et froid – année 2020",
  description = paste0("Consommations annuelles et nombre de points de livraison de gaz naturel, par secteur d'activité ",
                       "(agriculture, industrie, tertiaire, résidentiel et non affecté) ou selon le code NAF à 2 niveaux ",
                       "selon les cas, à la maille IRIS"),
  temporal_coverage_start = "2020-01-01",
  temporal_coverage_end = "2020-12-31",
  millesime = "2021-10"

)
