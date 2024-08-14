library(tidyverse)
library(sf)
library(httr)
library(sweapi)

# https://www.lantmateriet.se/sv/geodata/vara-produkter/produktlista/geografisk-indelning-direkt/
# https://www.lantmateriet.se/globalassets/geodata/geodatatjanster/tb_geografisk-indelning-direkt-v2.0.1.pdf

# Output formats ----
lm_geografisk_indelning("Län/AB?includeData=detaljeradGeometri&srid=3006", format = "sf")
lm_geografisk_indelning("Län/AB?includeData=detaljeradGeometri&srid=3006", format = "xml")
lm_geografisk_indelning("Län/AB?includeData=detaljeradGeometri&srid=3006", format = "json")

## Use rename option ----
# NOTE!!! Only tested for Lan and Kommun!
lm_geografisk_indelning("Län/AB?includeData=detaljeradGeometri&srid=3006", rename = FALSE)
lm_geografisk_indelning("Län/AB?includeData=detaljeradGeometri&srid=3006", rename = TRUE)

lm_geografisk_indelning("Kommun/0127?includeData=detaljeradUtanEnklaverGeometri&srid=3006", rename = FALSE)
lm_geografisk_indelning("Kommun/0127?includeData=detaljeradUtanEnklaverGeometri&srid=3006", rename = TRUE)

# Län ----
## Single county/län ----
lm_geografisk_indelning("Län/01?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Län/AB?includeData=detaljeradGeometri&srid=3006")

## All ----
lm_geografisk_indelning("Län?includeData=oversiktligGeometri&srid=3006")
lm_geografisk_indelning("Län?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Län?includeData=detaljeradUtanEnklaverGeometri&srid=3006")

# Kommun ----
## All ----
lm_geografisk_indelning("Kommun?includeData=oversiktligGeometri&srid=3006")
lm_geografisk_indelning("Kommun?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Kommun?includeData=detaljeradUtanEnklaverGeometri&srid=3006")

## Specified ----
lm_geografisk_indelning("Kommun/0127?includeData=detaljeradUtanEnklaverGeometri&srid=3006")
lm_geografisk_indelning("Kommun/namn/Östhammar?match=CONTAINS&includeData=detaljeradUtanEnklaverGeometri&srid=3006")
lm_geografisk_indelning("Kommun/namn/Östhammar?match=CONTAINS&includeData=detaljeradGeometri&srid=3006")

# Distrikt ----
lm_geografisk_indelning("Distrikt?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Distrikt/212090?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Distrikt/namn/botkyrka?match=CONTAINS&includeData=detaljeradGeometri")
lm_geografisk_indelning("Distrikt/namn/gävle?match=CONTAINS&includeData=detaljeradGeometri&srid=3012", 3012)

# Jordregistersocken ----
lm_geografisk_indelning("Jordregistersocken?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Jordregistersocken/33dd6bda-e9ef-4278-bfaa-9172065c550b?includeData=detaljeradGeometri&srid=3006")
lm_geografisk_indelning("Jordregistersocken/namn/botkyrka?match=CONTAINS&includeData=detaljeradGeometri&srid=3006")

# SCB-område ----
lm_geografisk_indelning("SCB-område/A0669?includeData=detaljeradGeometri&srid=3006")
# scb_omr <- lm_geografisk_indelning("SCB-område?includeData=detaljeradGeometri&srid=3006")
# scb_omr |> st_drop_geometry() |> count(typ)

# DEVELOPMENT ----
## POST ----

geometry <- '"
{
 "geometri": {
 "type": "Polygon",
 "crs": {
 "type": "name",
 "properties": {
 "name": "urn:ogc:def:crs:EPSG::3006"
 }
 },
 "coordinates": [
 [ [618174, 6728548], [618153, 6728423], [618270, 6728395],
 [618296, 6728525], [618174, 6728548] ]
 ]
 },
 "buffer": 50
}
"'

url <- "Län/geometri?includeData=detaljeradUtanEnklaverGeometri"

out <- httr::POST(
  url = file.path(base_url, url),
  config = httr::add_headers(
    .headers = c(
      sweapi:::add_headers_token(token),
      'Accept' = 'application/json')
  ),
  # httr::content_type_json(),
  body = geometry,
  encode = "json"
)
