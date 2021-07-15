# Tarea_orquideas
---
title: "Orquideas_Cr"
output: html_document
---

# Carga de los paquetes

```{r}
library(sf)
library(raster)
library(dplyr)
library(spData)

library(leaflet)
library(plotly)
library(DT)
```

# Carga de los datos

```{r}
orquideas<-
  st_read("https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/orchidaceae-cr-registros.csv",
          options = c(
            "X_POSSIBLE_NAMES=decimaLongitude",
            "Y_POSSIBLE_NAMES=decimaLatie"
          ),
          quiet =TRUE
  )
provincias <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_provincias_simp_wgs84.geojson",
  )
asp <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/sinac/asp/asp-wgs84.geojson",
    quiet = TRUE
  )
alt <- getData(
  "worldclim",
  var = "alt",
  res = .5,
  lon = -84,
  lat = 10
)
altitud <-
  alt %>%
  crop(provincias) %>%
  mask(provincias)
rcol <- colorNumeric(c("#00CC00", "#FFFF66", "#FF0000"),
                     values(altitud),
                     na.color = "transparent")
st_crs(asp) = 4326
st_crs(orq) = 4326

## filtros
orq$species[orq$species == ""] <- "No"

orq <- orq %>%
  filter(!is.na(coordinateUncertaintyInMeters) & coordinateUncertaintyInMeters <= 1000)%>%
  filter(species!="No")

asp <- asp %>%
  filter(descripcio!="Area Marina de Manejo"&descripcio!="Area marina protegida")
asp_registros <-
  asp %>%
  st_join(orq) %>%
  group_by(nombre_asp) %>%
  summarize(especies = n_distinct(species,na.rm = TRUE))

```
## Especies de orquiedeas repartidas en las diferentes areas protegidas de Costa Rica
El siguiente mapa muestra la cantidad de especies de orquideas en Costa rica repartidas en diferentes Areas Silvestres Protegidas (ASP) del pais.
## popups
```{r}
pasp <- paste0("<b>", "Area protegia: ","</b>",
                (asp_registros$nombre_asp),"<br>",
                "<b>","Area_km: ","</b>",
                (asp$area_km),"<br>",
                "<b>", "Cantidad de especies: ","</b>",
                (asp_registros$especies),"<br>",
                "<b>", "Descripcion ASP: ","</b>",
                (asp$descripcio))
# Paleta de colores
colores_registros <-
  colorNumeric(palette = "RdPu",
               domain = asp_registros$especies,
               na.color = "transparent")
# Mapa Leaflet
leaflet() %>%
      addTiles(group = "OSM") %>%
      addRasterImage(
          altitud, 
          colors = rcol, 
          opacity = 0.8,
          group = "Altitud")%>%
  addPolygons(
        data = asp_registros,
        fillColor = ~ colores_registros(asp_registros$especies),
        fillOpacity = 0.7,
        stroke = TRUE,
        color = "black",
        weight = 1,
        popup = pasp,
        group = "ASP - especies de orquideas"
      ) %>%
  addLayersControl(baseGroups = c("OSM"),
                       overlayGroups = c("ASP - especies de orquideas", "Altitud")) %>%
  addLegend(
        position = "bottomleft",
        pal = colores_registros,
        values = asp_registros$especies,
        group = "ASP - especies de orquideas",
        title = "Cantidad de especies") 
