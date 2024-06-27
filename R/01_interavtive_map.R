# lookup = file.path(getwd(), "R/lookup.fgb")
# if (!file.exists(lookup)){
#   .lookup_table = sf::st_read('http://s3.eu-central-1.wasabisys.com/gedi-ard/gedi_lookup.fgb', quiet=T)
#   sf::st_write(.lookup_table, lookup, quiet = T, delete_dsn = T)
# } else {
#   .lookup_table = sf::st_read(lookup, quiet=T)
# }
# rm(lookup)

mapview::mapviewOptions(basemaps = c('OpenStreetMap', 'Esri.WorldImagery'),
                        legend.pos = "bottomright")


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
    gedi_basemap,
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
