
#' Title
#'
#' @param nvrid
#' @param beslutsstatus
#' @param crs
#' @param crs_to
#' @param map
#' @param fill
#' @param base_url
#'
#' @returns
#' @export
#'
#' @examples
get_nvrid_wkt <- function(
    nvrid,
    beslutsstatus = "Gällande",
    crs = 3006,
    crs_to = NULL,
    map = FALSE,
    fill = TRUE,
    base_url = "https://geodata.naturvardsverket.se/naturvardsregistret/rest/v3/omrade"
) {

  wkt_str <- glue::glue("{base_url}/{nvrid}/{beslutsstatus}/wkt")
  wkt_resp <- purrr::map_chr(wkt_str, \(x) readLines(x, warn = FALSE))

  xy <- sf::st_as_sfc(wkt_resp, crs = crs) |>
    sf::st_as_sf() |>
    dplyr::mutate(
      nvrid = nvrid,
      beslutsstatus = beslutsstatus
    )

  sf::st_geometry(xy) <- "geom"

  if (!is.null(crs_to)) {
    xy <- xy |>
      sf::st_transform(crs_to)
  }

  if (map) {
    # Leaflet
    m <- leaflet::leaflet(
      data = xy |> sf::st_transform(4326)) |>
      leaflet::addTiles() |>
      leaflet::addPolygons(fill = fill)
    # Mapview
    # m <- mapview::mapview(xy)
    m
  } else {
    xy
  }
}

#' Title
#'
#' @param str_parameters
#' @param ...
#' @param base_url
#' @param remove_atom_link
#' @param convert_unix_timestamp
#'
#' @returns
#' @export
#'
#' @examples
nv_rest_api <- function(
    str_parameters,
    ...,
    base_url = "https://geodata.naturvardsverket.se/naturvardsregistret/rest/v3",
    remove_atom_link = FALSE,
    convert_unix_timestamp = TRUE) {

  p <- with(list(...), glue::glue(str_parameters))
  rest_url <- glue::glue("{base_url}/{p}")

  x <- purrr::map_dfr(
    rest_url,
    \(x) jsonlite::fromJSON(txt = x)
  ) |>
    tibble::as_tibble()

  if (convert_unix_timestamp) {
    # UNIX timestamp is in milliseconds
    x <- x |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::contains("datum") & tidyselect::where(is.numeric),
          \(x) lubridate::as_datetime(x/1000, tz = "Europe/Stockholm") |>
            lubridate::as_date()
          # \(x) anytime::anydate(x/1000)
        )
      )
  }

  if (remove_atom_link) {
    x <- x |>
      dplyr::select(-"atom.link")
  }

  x |>
    dplyr::distinct()
}

#' Title
#'
#' @param .x
#' @param base_url
#'
#' @returns
#' @export
#'
#' @examples
nv_rest_api_httr <- function(
    .x,
    base_url = "https://geodata.naturvardsverket.se/naturvardsregistret/rest/v3/omrade/nolinks"
) {
  .x |>
    purrr::pmap(list) |>
    purrr::map_chr(
      \(x) httr::modify_url(base_url, query = x)
    ) |>
    purrr::map_dfr(
      jsonlite::fromJSON
    ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::contains("datum"),
        \(x) lubridate::as_datetime(x/1000, tz = "Europe/Stockholm") |>
          lubridate::as_date()
      )
    )
}

#' Title
#'
#' @param path
#' @param node
#'
#' @returns
#' @export
#'
#' @examples
# https://stackoverflow.com/questions/72883605/how-to-download-several-datasets-from-website-stored-in-form-index-of-data-month
# https://stackoverflow.com/questions/70870646/scrape-data-within-a-table-from-webpage-using-rvest-in-r
get_index_nv_geodata <- function(path, node = "pre") {

  rvest::read_html(path) |>
    rvest::html_node(node) |>
    rvest::html_text() |>
    utils::read.table(
      text = _,
      sep = "\n",
      header = TRUE,
      quote = ""
    ) |>
    rlang::set_names("text") |>
    dplyr::mutate(
      text = stringr::str_squish(text)
    ) |>
    tidyr::separate_wider_delim(
      cols = text,
      delim = " ",
      names = c("file", "date", "time", "size")
    ) |>
    tidyr::unite(
      col = "last_modified",
      date, time,
      sep = " "
    ) |>
    dplyr::mutate(
      last_modified = readr::parse_datetime(
        last_modified,
        "%Y-%m-%d %H:%M"
      ),
      size_bytes = eagles::convert_bytes(size),
      file_type = tools::file_ext(file)
    )
}
