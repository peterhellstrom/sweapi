#' @export
lm_markhojd_point <- function(
    east,
    north,
    altitude_only = FALSE,
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/hojd/v1/rest/api/hojd/3006",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::GET(
    url = file.path(url, east, north),
    config = httr::add_headers(
      .headers = add_headers_token(token)
    )
  )

  if (!altitude_only) {
    httr::content(out, as = as)
  } else {
    httr::content(out)$geometry$coordinates[[3]]
  }
}

#' @export
lm_markhojd_geometry <- function(
    geometry,
    encode = "raw",
    as = "text",
    url = "https://api.lantmateriet.se/distribution/produkter/hojd/v1/rest/api/hojd",
    token = httr::content(lm_get_token())$access_token) {

  out <- httr::POST(
    url = url,
    config = httr::add_headers(
      .headers = c(
        add_headers_token(token),
        'Accept' = 'application/json')
    ),
    httr::content_type_json(),
    body = geometry,
    encode = encode)

  httr::content(out, as = as)
}
