# https://download-ver.lantmateriet.se/hojdmodell/wcs/v1?service=WCS&version=2.0.1&request=DescribeCoverage&coverageid=hojdgrid_1m

#' Title
#'
#' @param crs
#' @param xmin
#' @param ymin
#' @param xmax
#' @param ymax
#' @param url_base
#'
#' @returns
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .x
#' @param ruta
#' @param dx
#' @param dy
#' @param main_dir
#'
#' @returns
#' @export
#'
#' @examples
lm_hojdmodell_prepare <- function(
    .x, ruta = .x$ruta_5, dx = 5000, dy = 5000,
    main_dir = NULL) {

  .x |>
    dplyr::mutate(
      ruta_100 = extract_ruta_100( {{ ruta }} ),
      crs = 3006,
      extract_coords_from_ruta_5( {{ ruta }} )
    ) |>
    dplyr::mutate(
      xmax = .data$xmin + dx,
      ymax = .data$ymin + dy
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      file_dl = stringr::str_c(
        main_dir,
        .data$ruta_100,
        stringr::str_c(.data$ruta_5, ".tif"),
        sep = "/"
      ),
      url_dl = lm_hojdmodell_url(
        crs = .data$crs,
        xmin = .data$xmin,
        ymin = .data$ymin,
        xmax = .data$xmax,
        ymax = .data$ymax
      )
    )
}

#' Title
#'
#' @param .x
#' @param dx
#' @param dy
#' @param crs
#'
#' @returns
#' @export
#'
#' @examples
lm_hojdmodell_prepare_sf <- function(
    .x, dx = 5000, dy = 5000, crs = 3006) {

  purrr::map2(
    .x$xmin, .x$ymin,
    \(x, y) swecoords::grid_cell(x, y, delta_x = dx, delta_y = dy)
  ) |>
    sf::st_sfc(crs = crs) |>
    sf::st_sf(.x, geometry = _)
}


#' Title
#'
#' @param .x
#' @param user
#' @param password
#' @param create_dirs
#'
#' @returns
#' @export
#'
#' @examples
lm_hojdmodell_download <- function(
    .x,
    user = Sys.getenv("lm_atkomst_ver_user"),
    password = Sys.getenv("lm_atkomst_ver_pwd"),
    create_dirs = TRUE
) {

  if (create_dirs) {
    # Create download directories
    dl_dirs <- .x |>
      dplyr::select("file_dl") |>
      dplyr::mutate("file_dl" = dirname(.data$file_dl)) |>
      dplyr::distinct() |>
      dplyr::pull()

    fs::dir_create(dl_dirs)
  }

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
          password = password
        )
      )
    }
  )
}

# create virtual raster (vrt) from downloaded tif-files

#' Title
#'
#' @param .data
#' @param main_dir
#' @param out_vrt
#'
#' @returns
#' @export
#'
#' @examples
lm_hojdmodell_vrt <- function(
    .data,
    main_dir,
    out_vrt) {

  .data |>
    lm_hojdmodell_prepare(main_dir = .data$main_dir) |>
    dplyr::filter(fs::file_exists(.data$file_dl)) |>
    dplyr::pull(.data$file_dl) |>
    gdalUtilities::gdalbuildvrt(out_vrt) |>
    gdalUtilities::gdalinfo(mm = TRUE, stats = TRUE, hist = TRUE)
}
