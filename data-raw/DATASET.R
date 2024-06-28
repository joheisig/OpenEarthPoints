lookup = sf::read_sf("data/lookup.fgb")
reduced_cols = readLines("data/reduced_columns.txt")
gedi_cols = read.csv("data/gedi_columns.csv") |>
  dplyr::as_tibble()

usethis::use_data(lookup,
                  reduced_cols,
                  gedi_cols,
                  internal = T,
                  overwrite = TRUE)
