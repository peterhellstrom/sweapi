# Retrieve token

# 1) consumer_key and consumer_secret are generated in the API portal for a
# specified app.
# 2) then stored locally as environment variables; consumer_key and consumer_secret
# use e.g. cmd in Windows, setx variable value /M
#' @export
lm_get_token <- function(
    url = "https://api.lantmateriet.se/token",
    consumer_key = Sys.getenv("consumer_key"),
    consumer_secret = Sys.getenv("consumer_secret")
) {

  # basic_auth <- glue::glue("Basic {RCurl::base64(glue::glue('{consumer_key}:{consumer_secret}'))}") |>
  #   rlang::set_names("Authorization")

  consumer_key_secret <- paste(consumer_key, consumer_secret, sep = ":")
  basic_auth <- c("Authorization" = paste("Basic", RCurl::base64(consumer_key_secret), sep = " "))

  httr::POST(
    url = url,
    config = httr::add_headers(
      .headers = basic_auth),
    body = 'grant_type=client_credentials')
}

# httr::content(lm_get_token())$access_token

add_headers_token <- function(token, sep = " ") {
  c('Authorization' = paste("Bearer", token, sep = sep))
}
