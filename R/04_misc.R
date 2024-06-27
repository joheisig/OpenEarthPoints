#' Turn downloaded data into sf object
#'
#' Convenience function to create an sf object from a downloaded dataset.
#' Needs to be loaded into memory beforehand using `collect()`
#'
#' @param x table with column "longitude" and "latitude"
#'
#' @return `sf` object with CRS WGS84 (EPSG:4326)
#' @export
make_sf = function(x){
  sf::st_as_sf(x,
               coords=c("longitude","latitude"),
               crs=sf::st_crs(4326))
}

#' Display available data columns in the GEDI data
#'
#' Returns a tibble with all variables, their description, unit, scale, data type,
#' valid range, nodata value, and GEDI source (level 2A or 2B)
#'
#' @return tibble
#' @export
#'
#' @examples
#' show_gedi_columns()
show_gedi_columns = function(){
  gedi_cols
}

.rescale = function(n, x, scl){
  fac = scl[scl$Variable == n,]
  if (n == "shotnumber") x[[n]] = as.character(x[[n]])
  if (!is.na(fac$NoData)) x[[n]] = dplyr::na_if(x[[n]], as.integer(fac$NoData))
  if (fac$Scale < 1) x[[n]] = x[[n]] * fac$Scale
  return(x[n])
}

#' Rescale downloaded data
#'
#' GEDI data was transformed from float to integers by applying a scale factor
#' to save memory on the cloud bucket and speed up retrieval. Apply this function
#' to scale data back to meaningful (floating point) values.
#' For an example see `[download_gedi()]`.
#'
#' @param x in-memory GEDI data table
#'
#' @return rescaled data with all columns provided in `x`
#' @export
rescale_gedi = function(x){
  suppressWarnings({
  scl = dplyr::select(gedi_cols, Variable, Scale, NoData) |>
    dplyr::filter(Variable %in% names(x))
  purrr::map(names(x), .rescale, x = x, scl = scl) |>
    purrr::list_cbind()
  })
}
