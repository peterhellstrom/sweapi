# https://download-ver.lantmateriet.se/hojdmodell/wcs/v1?service=WCS&version=2.0.1&request=DescribeCoverage&coverageid=hojdgrid_1m
#' @export
lm_hojdmodell_url <- function(
    crs, xmin, ymin, xmax, ymax,
    url_base = "https://download-ver.lantmateriet.se/hojdmodell/wcs/v1") {

  httr::modify_url(
    url = url_base,
    query = list(
      service = "WCS",
      version = "2.0.1",
      request = "GetCoverage",
      coverageid = "hojdgrid_1m",
      subset = glue::glue("y,epsg:{crs}({ymin},{ymax})"),
      subset = glue::glue("x,epsg:{crs}({xmin},{xmax})"),
      format = "image/tiff"
    )
  )
}

# Note: use str_c() instead of file.path()
# to create paths, only way (?) if we want to include
# the optional argument main_dir = NULL
#' @export
lm_hojdmodell_prepare <- function(
    .x, ruta = ruta_5, dx = 5000, dy = 5000,
    main_dir = NULL) {

  .x |>
    dplyr::mutate(
      ruta_100 = extract_ruta_100({{ruta}}),
      crs = 3006,
      extract_coords_from_ruta_5({{ruta}}),
      xmax = xmin + dx,
      ymax = ymin + dy) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      file_dl = stringr::str_c(
        main_dir,
        ruta_100,
        stringr::str_c(ruta_5, ".tif"),
        sep = "/"),
      url_dl = lm_hojdmodell_url(
        crs = crs,
        xmin = xmin, ymin = ymin,
        xmax = xmax, ymax = ymax)
    )
}

#' @export
lm_hojdmodell_prepare_sf <- function(
    .x, dx = 5000, dy = 5000, crs = 3006) {
  purrr::map2(
    .x$xmin, .x$ymin,
    \(x, y) grid_cell(x, y, delta_x = dx, delta_y = dy)) |>
    sf::st_sfc(crs = crs) |>
    sf::st_sf(.x, geometry = _)
}

#' @export
lm_hojdmodell_download <- function(
    .x,
    user = Sys.getenv("lm_atkomst_ver_user"),
    password = Sys.getenv("lm_atkomst_ver_pwd")) {

  # Create download directories
  dl_dirs <- .x |>
    dplyr::select(file_dl) |>
    dplyr::mutate(file_dl = dirname(file_dl)) |>
    dplyr::distinct() |>
    dplyr::pull()

  fs::dir_create(dl_dirs)

  # Download data
  # Does remotes::download has an overwrite option?
  purrr::walk2(
    .x = .x$url_dl,
    .y = .x$file_dl,
    \(x,y) {
      remotes:::download(
        path = y,
        url = x,
        basic_auth = list(
          user = user,
          password = password))
    }
  )
}

# create virtual raster (vrt) from downloaded tif-files
#' @export
lm_hojdmodell_vrt <- function(
    .data,
    main_dir,
    out_vrt) {

  .data |>
    hojdmodell_prepare(main_dir = main_dir) |>
    dplyr::filter(file_exists(file_dl)) |>
    dplyr::pull(file_dl) |>
    gdalbuildvrt(out_vrt) |>
    gdalinfo(mm = TRUE, stats = TRUE, hist = TRUE)
}
