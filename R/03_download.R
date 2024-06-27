.download_tile = function(i, urls, nms, dir){
  utils::download.file(urls[i], file.path(dir, nms[i]))
}

.download_tile_windows_parellel = function(urls, nms, dir, cores, progress, timeout){
  cl = parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  opts = NULL
  if (progress){
    pb = utils::txtProgressBar(max=length(nms), style=3)
    on.exit(close(pb))
    prog = function(n) utils::setTxtProgressBar(pb, n)
    opts = list(progress=prog)
  }
  foreach::foreach(i = 1:length(nms), .options.snow=opts) %dopar% {
    withCallingHandlers({
      opts = options()
      options(timeout = timeout)
      utils::download.file(urls[i], file.path(dir, nms[i]))
      options(opts)
    }, error = function(e) stop("Download failed."))
  }
  parallel::stopCluster(cl)
}


#' Download GEDI data
#'
#' Download all GEDI data requested in a bbox_query or tile_query,
#' created using the respective function.
#' Data is written to disk in a specified directory and can be opened
#' using `[arrow::open_dataset(out.dir)] |> [dplyr::collect()]`
#'
#' @param x Output of `[bbox_query()]` or `[tile_query()]`
#' @param out.dir Target directory path for download
#' @param cores Specify cores > 1 for parallel processing (integer)
#' @param progress logical, show a progress bar?
#' @param require.confirmation logical, warn and require interactive confirmation for tile downloads with over 10 mio points
#' @param timeout Download time out in seconds
#'
#' @return none
#' @export
#'
#' @examples
#' # Construct a query for a small bbox
#' bb = sf::st_bbox(c(xmin=6, ymin=42, xmax=6.1, ymax=42.1), crs=4326)
#' bbq = bbox_query(bb, years=2020)
#'
#' # Define a directory and download
#' out.dir = file.path(tempdir(), "gedi")
#' download_gedi(bbq, out.dir = out.dir, timeout = 300)
#'
#' # Read data into memory and rescale
#' arrow::open_dataset(out.dir) |>
#'   head() |>
#'   dplyr::collect() |>
#'   rescale_gedi()
#'
#' # cleanup
#' unlink(out.dir, recursive = TRUE)
download_gedi = function(x, out.dir = NULL, cores = 1, progress = T,
                         require.confirmation = T, timeout = 500){
  if (is.null(out.dir)) out.dir = file.path(getwd(), "GEDI_download")
  if (!dir.exists(out.dir)) dir.create(out.dir, recursive = T)
  t = NULL
  # download entire tiles (x = table)
  if (all(c("lon", "lat", "year", "dir") %in% names(x))){
    n = sum(x$n_points)
    answ = "y"
    if (require.confirmation & n > 1e07){
      answ = as.character(readline(
        prompt = paste("You are about to download", nrow(x),
                       "tiles with", round(n/1e06,1),
                       "million points. Do you want to continue? (y/n)")))
    }
    if (tolower(answ) %in% c("", "y")){
      message("Downloading tiles...")
      # construct URLs and filenames
      b = "https://s3.eu-central-1.wasabisys.com/gedi-ard/level2/l2v002.gedi_20190418_20230316_go_epsg.4326_v20240614"
      urls = paste0(b, x$dir)
      nms = paste0("GEDI_tile", gsub("/", "_", dirname(x$dir)), ".parquet")
      backup_options = options()
      options(timeout = timeout)
      t = system.time({
        # sequential
        if (cores==1){
          f = purrr::map(1:length(urls), .download_tile, urls = urls, nms = nms,
                         dir = out.dir, .progress = progress)
          # Windows parallel
        } else if (Sys.info()["sysname"] == "Windows"){
          f = .download_tile_windows_parellel(urls = urls, nms = nms, dir = out.dir,
                                              cores = cores, progress = progress,
                                              timeout = timeout)
          # Linux parallel
        } else {
          if (progress) {
            f = pbmcapply::pbmclapply(1:length(urls), .download_tile, urls = urls,
                                      nms = nms, dir = out.dir, mc.cores = cores)
          } else {
            f = parallel::mclapply(1:length(urls), .download_tile, urls = urls,
                                   nms = nms, dir = out.dir, mc.cores = cores)
          }
        }
        f = unlist(f)
        options(backup_options)
      })
    } else {
      message("Download aborted.")
    }
  }
  # download partial tiles (queries)
  if (all(class(x) == "list" & class(x[[1]]) == "RPolarsLazyFrame")){
    message("Downloading bbox subsets...")
    nms = file.path(out.dir, paste0(names(x), ".parquet"))
    t = system.time({
      purrr::map2(x, nms, \(x,n) x$collect()$write_parquet(n), .progress = progress)
    })
  }
  if (!is.null(t)) message("Completed after ", round(t["elapsed"]), " sec.")
}
