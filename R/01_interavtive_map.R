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
