lookup = sf::read_sf("data/lookup.fgb")
reduced_cols = readLines("data/reduced_columns.txt")
gedi_cols = read.csv("data/gedi_columns.csv")

ltm = c(
  list("N pts [mio]" = lookup[1:2,]),
  lookup |>
    dplyr::mutate(n_points=n_points/1e06) |>
    split(lookup$year)
)
gedi_basemap = mapview::mapview(ltm, zcol = "n_points",
                                 at = 0:9, lwd=0,
                                 label = c(F, rep("n_points", 5)),
                                 hide = c(F,F,T,T,T,T),
                                 homebutton = rep(F, 6),
                                 legend = c(T,F,F,F,F,F),
                                 layer.name = "GEDI L2 Count",
                                 popup = rep(F, 6))
rm(ltm)

usethis::use_data(lookup,
                  reduced_cols,
                  gedi_cols,
                  gedi_basemap,
                  internal = T,
                  overwrite = TRUE)
