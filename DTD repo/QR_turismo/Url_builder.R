
install.packages("ggplot2")
install.packages("ggmap")
install.packages("maptools")
install.packages("maps")
install.packages("sp")
install.packages("RColorBrewer")
install.packages("leaflet")
install.packages("stplanr")

library(sf)
library(tidyr)
library(dplyr)
library(stplanr)
library(leaflet)
library(ggplot2)
library(ggmap)
library(maptools)
# library(maps)
library(sp)
library(RColorBrewer)


library(readxl)
Agenda_Verano <- read_excel("Agenda Verano.xlsx")
View(Agenda_Verano)

colnames(Agenda_Verano) <- gsub(" ", ".", colnames(Agenda_Verano))

# asignamos categorias

eventos <- c("Naturaleza y Ecoturismo", "Cultural e Histórico", "Gastronomía", "Deporte y Aventura")

Agenda_Verano$Grupo <- sample(eventos, nrow(Agenda_Verano), replace = TRUE)

rm(eventos)

# hora inicio
Agenda_Verano$hora_inicio <- format(Agenda_Verano$Fecha.inicio, "%H:%M:%S")

#fecha
Agenda_Verano$fecha <- as.Date(Agenda_Verano$Fecha.inicio)

# hora_inicio + 1h
Agenda_Verano$hora_fin <- format(as.POSIXct(Agenda_Verano$hora_inicio, format = "%H:%M:%S") + 3600, "%H:%M:%S")


leaflet() %>%
  addTiles() %>%
  addMarkers(data = Agenda_Verano, lng = ~Longitud, lat = ~Latitud, popup = "Location")


writexl::write_xlsx(Agenda_Verano, paste0(getwd(), "/Agenda_Verano_R.xlsx"))



# filtro dia
agenda_dia <- Agenda_Verano[80, ]

agenda_dia <- Agenda_Verano[80, ] %>%
  dplyr::group_by(fecha) %>%
  dplyr::mutate(latlong = paste0(Latitud, ",", Longitud)) %>%
  dplyr::mutate(
    waypoints = paste(latlong, collapse = "/"),
    plaza_independencia = "-32.889756,-68.844608",  # Remove extra spaces
    url = paste0("https://www.google.com/maps/dir/"," /", waypoints, "/@", plaza_independencia)
  ) %>%
  dplyr::ungroup()  # Ensure we ungroup at the end




## construir la url manualmente

agenda_dia <- Agenda_Verano[80, ] %>%
  mutate(latlong = paste0(Latitud, ",", Longitud))

waypoints <- paste0(paste(agenda_dia$latlong, collapse = "/")) # eventos

base_url <- "https://www.google.com/maps/dir/"


# alojamiento
# centroid <- st_centroid(st_union(agenda_dia))
alojamiento <- paste0(st_coordinates(centroid)[ , "Y"], ",", st_coordinates(centroid)[ , "X"], collapse = ",") # puse la mediana de todos los eventos (por si no quiere poner alojamiento)

url <- paste0(base_url, paste0(alojamiento , "/"), waypoints, paste0("/@", alojamiento))

# plaza_independencia
plaza_independencia <- "-32.889756,-68.844608"

url <- paste0(base_url, paste0(plaza_independencia , "/"), waypoints, paste0("/@", plaza_independencia))




# Travel route optim ------------------------------------------------------


### intento de obtener direcciones desde el server OSRM

library(httr)
library(jsonlite)
library(sf)
library(leaflet)

# Define the OSRM server URL
osrm_server <- "http://router.project-osrm.org"

# Define the waypoints (coordinates)
waypoints <- list(
  c(-68.844608, -32.889756),  # Start point
  c(-68.8358516, -32.8572113) # End point
)

# Construct the URL
coordinates <- paste(sapply(waypoints, function(coord) paste(coord, collapse = ",")), collapse = ";")
url <- paste0(osrm_server, "/route/v1/driving/", coordinates, "?overview=full&geometries=geojson")

# Fetch and parse the directions
response <- GET(url)
directions <- content(response, as = "parsed", type = "application/json")

# Decode the geometry
route_geometry <- directions$routes[[1]]$geometry

# Create a GeoJSON feature collection
geojson <- list(
  type = "FeatureCollection",
  features = list(
    list(
      type = "Feature",
      geometry = route_geometry,
      properties = list()
    )
  )
)

route_sf <- geojsonsf::geojson_sf(jsonlite::toJSON(geojson, auto_unbox = TRUE))

# Plot the route using leaflet
leaflet() %>%
  addTiles() %>%
  addPolylines(data = route_sf, color = "blue") %>%
  addMarkers(lng = -68.844608, lat = -32.889756, popup = "Start") %>%
  addMarkers(lng = -68.8358516, lat = -32.8572113, popup = "End")








### Intento optimizar ruta de viaje (conclyó en el trazado de la ruta sin optimización)
agenda_dia <- Agenda_Verano %>% filter(fecha == as.Date(Agenda_Verano$fecha[80]))

# redefinimos a Posixct
agenda_dia$hora_inicio <- as.POSIXct(agenda_dia$Fecha.inicio)
agenda_dia$hora_fin <- as.POSIXct(agenda_dia$Fecha.inicio)


# shapefile
agenda_dia <- st_as_sf(agenda_dia, coords = c("Longitud", "Latitud"), crs = 4326)

centroid <- st_centroid(st_union(agenda_dia))
centroid_lon <- st_coordinates(centroid)[, "X"]
centroid_lat <- st_coordinates(centroid)[, "Y"]

agenda_dia$long <- st_coordinates(agenda_dia)[, "X"]
agenda_dia$lat <- st_coordinates(agenda_dia)[, "Y"]

leaflet(data = agenda_dia) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolylines(lng = c(agenda_dia$long, centroid_lon), 
               lat = c(agenda_dia$lat, centroid_lat),
               color = "red") %>% 
  addMarkers(data = agenda_dia, lng = ~long, lat = ~lat, popup = "Location") %>% 
  addMarkers(data = centroid)




















