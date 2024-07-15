# https://r-pkgs.org

# devtools::install_github("r-lib/devtools")
# devtools::install_github("r-lib/usethis")

library(devtools)

# p <- "W:/projects/R/sweapi"
# usethis::create_package(p, check_name = FALSE)

load_all()

# Must run document() to add export functions to NAMESPACE
document()

chk_pkg <- check()
dplyr::glimpse(chk_pkg)
names(chk_pkg)

test()

usethis::use_mit_license()

use_git_config(
  user.name = "peterhellstrom",
  user.email = "peter.hellstrom@nrm.se"
)

usethis::use_git()
usethis::use_github()

usethis::create_github_token()

use_readme_rmd()
build_readme()

# Imports ----

usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("fs", min_version = TRUE)
usethis::use_package("gdalUtilities", min_version = TRUE)
usethis::use_package("glue", min_version = TRUE)
usethis::use_package("httr", min_version = TRUE)
usethis::use_package("jsonlite", min_version = TRUE)
usethis::use_package("leaflet", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)
usethis::use_package("RCurl", min_version = TRUE)
usethis::use_package("remotes", min_version = TRUE)
usethis::use_package("sf", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("tibble", min_version = TRUE)
usethis::use_package("tidyselect", min_version = TRUE)

## non-CRAN packages ----
usethis::use_dev_package("swecoords", remote = "github::peterhellstrom/swecoords")

usethis::use_tidy_description()

# Ignore ----
usethis::use_build_ignore(c("backup", "data-raw", "development", "examples"))

# Document data:
# https://r-pkgs.org/data.html

# Install ----
install()

# install_github("peterhellstrom/sweapi")

## Load package ----
library(sweapi)

## Data sets ----
usethis::use_data_raw()
