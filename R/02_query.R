.five_deg = function(x){
  floor(x / 5)*5
}

#' Find tiles intersecting your bbox
#'
#' Intersect your bbox with a lookup table for 5x5 degree tiles, which can be used
#' for download.
#'
#' @param x `sf` / `bbox` object (output of [`draw_bbox()`])
#' @param years integer vector of length > 0, since 2019
#'
#' @return `sf` object, subset of tile lookup table with tiles of interest
#' @export
#'
#' @examples
#' bb = sf::st_bbox(c(xmin=6, ymin=42, xmax=8, ymax=44), crs=4326)
#' tile_query(bb, years=2020:2022)
tile_query = function(x, years = 2019:2023){  #, verbose = F
  if ("sf" %in% class(x)){
    stopifnot("The provided bbox should be in CRS EPSG:4326 (WGS84)." = sf::st_crs(x) == sf::st_crs(4326))
    # test for other geometry types than polygon
    tiles = lookup |> sf::st_filter(x)
    x_bb = sf::st_bbox(x) |> as.list()
  } else if ((!"sf" %in% class(x)) & all(names(x) %in% c("xmin", "ymin", "xmax", "ymax"))){
    x_bb = as.list(x)
    tiles = dplyr::filter(lookup,
                          dplyr::between(lon, .five_deg(x_bb$xmin), .five_deg(x_bb$xmax)),
                          dplyr::between(lat, .five_deg(x_bb$ymin), .five_deg(x_bb$ymax)))
  } else if (class(x) %in% c("tibble", "data.frame") & all(names(x) %in% c("lat","lon"))){

    x_expand = dplyr::mutate(x, lon = .five_deg(lon), lat = .five_deg(lat)) |>
      dplyr::distinct()
    tiles = dplyr::inner_join(lookup, x_expand, by = dplyr::join_by(lon, lat))
    x_bb = c(range(x$lon), range(x$lat))[c(1,3,2,4)] |>
      stats::setNames(c("xmin", "ymin", "xmax", "ymax"))
  } else {
    stop("Could not derive a list of tiles from input x. Make sure it is an sf bbox/polygon in lat/lon or a data.frame with columns named lat & lon.")
  }

  tiles = dplyr::filter(tiles, year %in% years)
  #if (verbose) message("Selected ", nrow(tiles), " complete tiles within the bounding box.")
  attr(tiles, "bbox") = x_bb
  return(tiles)
}

.build_bbox_query = function(t, cols, bbox, filters = NULL){
  q = polars::pl$scan_parquet(t)$
    filter(polars::pl$col("longitude")$is_between(bbox$xmin, bbox$xmax),
           polars::pl$col("latitude")$is_between(bbox$ymin, bbox$ymax)
    )
  if (!is.null(cols)) q = q$select(cols)
  #if (!is.null(filters))
  q
}

#' Create data query for your bounding box
#'
#' @param x `sf` / `bbox` object (output of [`draw_bbox()`])
#' @param years integer vector of length > 0, since 2019
#' @param columns Set of columns to retrieve. Can be "all" (default), "reduced" (n=34), or character vector.
#'
#' @return list of Polars queries to be passed to [`download_gedi()`]
#' @export
#'
#' @examples
#' bb = sf::st_bbox(c(xmin=6, ymin=42, xmax=6.1, ymax=42.1), crs=4326)
#' bbox_query(bb, years=2020)
bbox_query = function(x, years = 2019:2023, columns = "all"){
  tls = tile_query(x, years = years)
  bbox = attr(tls, "bbox") |> as.list()
  b = "https://s3.eu-central-1.wasabisys.com/gedi-ard/level2/l2v002.gedi_20190418_20230316_go_epsg.4326_v20240614"
  urls = paste0(b, tls$dir)
  nms = paste0("GEDI_tile_subset", gsub("/", "_", dirname(tls$dir)))

  if (columns == "all") {
    cols = NULL
  } else if (columns == "reduced"){
    cols = reduced_cols
  } else if (length(columns) > 1){
    cols = columns
  }
  queries = purrr::map(urls, .build_bbox_query, cols, bbox)
  names(queries) = nms
  return(queries)
}
