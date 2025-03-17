#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
extract_ruta_100 <- function(x) {
  stringr::str_c(
    stringr::str_sub(x, 1, 2),
    stringr::str_sub(x, 5, 5),
    sep = "_"
  )
}

#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @examples
extract_coords_from_ruta_5 <- function(x) {

  ymin <- stringr::str_c(
    stringr::str_sub(x, 1, 3),
    stringr::str_sub(x, 8, 8),
    "000"
  )

  xmin <- stringr::str_c(
    stringr::str_sub(x, 5, 6),
    stringr::str_sub(x, 9, 9),
    "000"
  )

  data.frame(
    xmin = as.numeric(xmin),
    ymin = as.numeric(ymin)
  )
}
