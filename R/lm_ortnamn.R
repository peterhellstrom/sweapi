#' @export
lm_ortnamn_kriterier <- function(
    query,
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/ortnamn/v2.1",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::GET(
    url = httr::modify_url(file.path(url, "kriterier"), query = query),
    config = httr::add_headers(
      .headers = add_headers_token(token)
    )
  )

  httr::content(out, as = as)
}

# two different id columns - why?
# Input x is a list (parsed json-object)
#' @export
lm_ortnamn_unnest_parsed <- function(x, crs = 3006) {
  tibble::tibble(data = x)[5,] |>
    tidyr::unnest("data") |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_wider("properties", names_sep = "_") |>
    tidyr::unnest("properties_placering") |>
    tidyr::unnest_wider("properties_placering") |>
    tidyr::unnest_wider("punkt", names_sep = "_") |>
    tidyr::unnest_wider("punkt_coordinates", names_sep = "_") |>
    dplyr::select(-"type", -"bbox", -"geometry", -"punkt_type") |>
    dplyr::rename(
      "namn" = "properties_namn",
      "sprak" = "properties_sprak",
      "easting" = "punkt_coordinates_1",
      "northing" = "punkt_coordinates_2"
    ) |>
    sf::st_as_sf(
      coords = c("easting", "northing"),
      crs = crs
    )
}

# type
# crs
## type
# bbox
# totaltAntal
# features
## type
## bbox
## id
## geometry
## properties
### id
### namn
### sprak
### placering
#### lankod
#### lannamn
#### kommunkod
#### kommunnamn
#### sockenstadkod
#### sockenstadnamn
#### namntyp
#### punkt
##### coordinates
##### type

# Wrapper function
#' @export
lm_ortnamn_coords <- function(
    easting,
    northing,
    crs = 3006,
    token = httr::content(lm_get_token())$access_token) {

  lm_ortnamn_kriterier(
    token = token,
    query = list(
      punkt = stringr::str_c(northing, easting, sep = ","),
      punktSrid = crs),
    as = "parsed"
  ) |>
    lm_ortnamn_unnest_parsed()
}
