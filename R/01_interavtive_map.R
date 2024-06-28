mapview::mapviewOptions(basemaps = c('OpenStreetMap', 'Esri.WorldImagery'),
                        legend.pos = "bottomright")
load("R/sysdata.rda")
ltm = c(
  list("N pts [mio]" = lookup[1:2,]),
  lookup |>
    dplyr::mutate(n_points=n_points/1e06) |>
    split(lookup$year)
)
.gedi_basemap = mapview::mapview(
  ltm,
  zcol = "n_points",
  at = 0:9, lwd=0,
  label = c(F, rep("n_points", 5)),
  hide = c(F,F,T,T,T,T),
  homebutton = rep(F, 6),
  legend = c(T,F,F,F,F,F),
  layer.name = "GEDI L2 Count",
  popup = rep(F, 6))
rm(ltm)

#' Draw a bounding box
#'
#' Open an interactive map and select a rectangular Area of Interest for later
#' download of point data. The underlying map shows the total number GEDI points
#' per 5x5 degree tile with map layers for each year.
#'
#' @return `sf` bbox object with Coordinate Reference System
#' @export
#'
#' @examples
#' \dontrun{
#' bb = draw_bbox()
#' print(bb)
#' sf::st_area(bb)
#' }
draw_bbox = function(){
  f = mapedit::drawFeatures(
    .gedi_basemap,
    title = "Draw a Bounding Box",
    editorOptions = list(
      position = "bottomleft",
      polylineOptions = FALSE,
      polygonOptions = FALSE,
      circleOptions = FALSE,
      rectangleOptions = leaflet.extras::drawRectangleOptions(),
      markerOptions = FALSE,
      singleFeature = FALSE,
      circleMarkerOptions = FALSE,
      editOptions = FALSE))
  return(f[nrow(f),])
}
