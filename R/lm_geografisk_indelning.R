#' Title
#'
#' @param request
#' @param crs
#' @param format
#' @param url
#' @param token
#'
#' @returns
#' @export
#'
#' @examples
lm_geografisk_indelning <- function(
    request,
    crs = 3006,
    rename = FALSE,
    format = c("sf", "json", "xml"),
    url = "https://api.lantmateriet.se/distribution/produkter/geografiskindelning/v2",
    token = httr::content(lm_get_token())$access_token
) {

  format <- match.arg(format)

  response_format <- dplyr::case_when(
    format == "sf" ~ "application/json",
    TRUE ~ str_c('application', format, sep = "/")
  )

  response <- httr::GET(
    url = file.path(url, request),
    config = httr::add_headers(
      .headers = sweapi:::add_headers_token(token),
      'Accept' = response_format
    )
  )

  response_txt <- httr::content(response, as = "text")

  if (format %in% c("xml", "json")) {
    response_txt
  } else {
    response_sf <- sf::read_sf(response_txt)
    crs_input <- sf::st_crs(crs)

    geometry_column_name <- dplyr::case_when(
      stringr::str_detect(request, "^Län") ~ "lansyta",
      stringr::str_detect(request, "^Kommun") ~ "kommunyta",
      stringr::str_detect(request, "^Distrikt") ~ "distriktsyta",
      stringr::str_detect(request, "^Jordregistersocken") ~ "sockenyta",
      stringr::str_detect(request, "^SCB-område") ~ "omradesyta",
      TRUE ~ NA_character_
    )

    geometry_column <- geojsonsf::geojson_sf(
      response_sf[[geometry_column_name]],
      input = crs_input$input,
      wkt = crs_input$wkt
    )

    out <- sf::st_as_sf(
      tibble::tibble(
        response_sf |>
          sf::st_drop_geometry() |>
          dplyr::select(- {{ geometry_column_name }} ),
        geometry_column
      )
    )
    if (rename) {
      out <- rename_geografisk_indelning(out)
    }
    out
  }
}

#' Title
#'
#' @param .data
#' @param .fix_names
#'
#' @returns
#' @export
#'
#' @examples
rename_geografisk_indelning <- function(
    .data,
    .fix_names = c(
      detaljtyp = "typ",
      lankod = "lanskod",
      lanbok = "lansbokstav",
      lannamn = "lansnamn",
      geom = "geometry",
      geom = "Lansyta",
      geom = "Kommunyta"
    )
) {
  .data |>
    dplyr::rename(any_of(.fix_names)) |>
    dplyr::mutate(
      detaljtyp = toupper(detaljtyp)
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("^lannamn"), \(x) stringr::str_remove(x, "(?:s)? län")
      ),
      dplyr::across(
        tidyselect::matches("^kommunkod"), \(x) stringr::str_sub(kommunkod, 1, 2),
        .names = "lankod"
      ),
      .after = detaljtyp
    )
}

#' Title
#'
#' @param .file
#' @param .detaljtyp
#' @param .fix_names
#'
#' @returns
#' @export
#'
#' @examples
rename_geografisk_indelning_gml <- function(
    .file,
    .detaljtyp,
    .fix_names = c(
      detaljtyp = "typ",
      lankod = "lanskod",
      lanbok = "lansbokstav",
      lannamn = "lansnamn",
      geom = "geometry",
      geom = "Lansyta",
      geom = "Kommunyta",
      id = "gml_id"
    )
) {

  out <- sf::read_sf(.file) |>
    dplyr::select(-c(lowerCorner, upperCorner)) |>
    dplyr::rename(any_of(.fix_names)) |>
    dplyr::mutate(
      detaljtyp = .detaljtyp,
      dplyr::across(
        tidyselect::matches("^lannamn"), \(x) stringr::str_remove(x, "(?:s)? län")
      ),
      dplyr::across(
        tidyselect::matches("^lankod"), \(x) stringr::str_pad(lankod, width = 2, pad = "0")
      ),
      dplyr::across(
        tidyselect::matches("^kommunkod"), \(x) str_pad(kommunkod, width = 4, pad = "0")
      ),
      dplyr::across(
        tidyselect::matches("kommunkod"), \(x) stringr::str_sub(kommunkod, 1, 2),
        .names = "lankod"
      ),
      dplyr::across(
        tidyselect::matches("^id"), \(x) stringr::str_remove(x, "ID_")
      )
    ) |>
    dplyr::relocate(detaljtyp, .before = lankod)

  if (.detaljtyp == "KOMMUN") {
    out <- out |> dplyr::relocate(detaljtyp, lankod, .after = 1L)
  }

  out
}
