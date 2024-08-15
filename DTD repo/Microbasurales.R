


## MICROBASURALES

library(dbscan)
library(raster)
library(gifski)
library(plotly)
library(rnaturalearth)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(ggmap)
library(writexl)
library(xml2)
library(httr)
library(jsonlite)
library(rvest)
library(tidyr)
library(dplyr)
library(pacman)
library(sf)
library(leaflet)
library(tmap)
library(plotly)
library(httr)
library(lubridate)
library(grid)


## LIENZO BASE


token <- httr::content(httr::POST("https://webgis.ciudaddemendoza.gob.ar/portal/sharing/rest/generateToken",
                                  body = list(
                                    username = "cchavarini",
                                    password = "",
                                    referer = "webgis.ciudaddemendoza.gob.ar/portal",
                                    f = "json"),
                                  encode = "form"))$token

# Barrios
url <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services/Catastro/MapServer/10/query"

params <- list(
  where = "1=1",
  outFields = "*",
  returnGeometry = "true",
  f = "geojson",
  token = token
)

# Make the HTTP GET request
response <- GET(url, query = params)

# Check the status code of the response
status_code(response)
# Extract the content from the response (para json)
content <- content(response, "text")

# read geojson object (para geojson)
map_barrios <- read_sf(content)


### Fracciones censales

url <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services/Demarcaci%C3%B3n_y_Divisiones/MapServer/5/query"

params <- list(
  where = "1=1",
  outFields = "*",
  returnGeometry = "true",
  f = "geojson",
  token = token
)

# Make the HTTP GET request
response <- GET(url, query = params)

# Check the status code of the response
status_code(response)

# Extract the content from the response (para json)
content <- content(response, "text")

# read geojson object (para geojson)
fracc_cens_ciudad <- read_sf(content) %>% filter(depto == "007")


# Microbasurales

shapefile_path <- "C:/Users/arisalomon/Desktop/DTD local/microbasurales/Microbasurales_geometria_correg/Microbasurales_geometria_correg.shp"

microbasurales <-st_read(shapefile_path)

microbasurales <- microbasurales[-which(st_is_empty(microbasurales)), ]


ggplot() +
  geom_sf(data = fracc_cens_ciudad, col = "black", alpha = 0, size = 0.1) +
  geom_sf(data = microbasurales, size = 5) +
  theme_minimal() +
  xlim(c(68.94, 68.88)) +
  ylim(c(32.91, 32.87))


leaflet() %>%
  addTiles() %>%
  setView(lng = -68.9000, lat = -32.8895, zoom = 13.5) %>%
  addPolygons(data = st_transform(microbasurales, crs = 4326), stroke = T)



# Opcional -> filtrar barrios en microbasurales ---------------------------



# ## Filtramos un barrio en microbasurales
# microbasurales$barrio <- NA
# 
# # identificamos centroide de cada polígono
# microbasurales$centroide <- st_centroid(microbasurales)$geometry
# 
# # compatibilizamos georeferencia
# map_barrios <- st_transform(map_barrios, st_crs(microbasurales))
# 
# # Loop through each point and check if it lies within any polygon
# for (i in 1:nrow(microbasurales)) {
#   
#   # Check if the point is within any polygon
#   points_within_barrio <- st_intersects(microbasurales[i, "centroide"], map_barrios$geometry, sparse = FALSE)
#   
#   if (any(points_within_barrio)) {
#     # If the point is within any polygon, store the ID of the first polygon it intersects with
#     microbasurales$barrio[i] <- map_barrios$nombre[which(points_within_barrio)[1]]
#   }
#   
#   print(paste0(round(i/nrow(microbasurales)*100, 2), "%"))
#   
# }
# 
# rm(points_within_barrio, i)
# 
# ggplot() +
#   geom_sf(data = fracc_cens_ciudad, col = "black", alpha = 0, size = 0.1) +
#   geom_sf(data = map_barrios, col = "black", alpha = 0, size = 0.1) +
#   geom_sf(data = microbasurales %>% 
#             filter(barrio == "31 de Mayo"), aes(col = barrio)) +
#   theme_minimal() +
#   xlim(c(68.94, 68.88)) +
#   ylim(c(32.91, 32.87))
# 
# 
# microbasurales <- microbasurales %>%
#   filter(barrio == "31 de Mayo")




# DBSCAN ~ pointgrid ------------------------------------------------------


# prueba poligono -> pointgrid

x <- 10
polygon <- microbasurales[x, ]

regular_points <- st_as_sf(as(raster(extent(polygon), res = 0.2), "SpatialPoints"))

plot(regular_points)

st_crs(regular_points) <- st_crs(polygon)

points_within_polygon <-  st_intersection(regular_points, polygon)

plot(points_within_polygon)

# Loop para iterar sobre cada polígono y guardar un pointgrid en lista

rm(x, polygon, regular_points, poly_extent, points_within_polygons)

points_within_polygons <- list()

for (x in 1:nrow(microbasurales)) {
  tryCatch({
    polygon <- microbasurales[x, ]
    
    poly_extent <- st_bbox(polygon)  # Extract bounding box of the polygon
    
    # Attempt to create regular grid of points covering the extent of the polygon
    regular_points <- st_as_sf(as(raster(extent(poly_extent), res = 0.2), "SpatialPoints"))
    
    # Set CRS
    st_crs(regular_points) <- st_crs(polygon)
    
    # Filter points within polygon and store the object in list
    points_within_polygons[[x]] <- st_intersection(regular_points, polygon)
  }, error = function(e) {
    # If an error occurs, print a message and continue to the next iteration
    cat("Error in iteration", x, ":", conditionMessage(e), "\n")
  })
  
  print(paste0(round(x/nrow(microbasurales)*100, 2), "%"))
  
}

combined_sf <- do.call(rbind, points_within_polygons)

leaflet(data = st_transform(combined_sf, crs = 4326)) %>%
  setView(lng = -68.9000, lat = -32.8895, zoom = 13.5) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(radius = 0.1,  # Size of the markers
                   color = "red",  # Color of the markers
                   stroke = FALSE,  # No border
                   fillOpacity = 0.7  # Opacity of the markers
  )

combined_sf$cluster <- as.factor(dbscan(st_coordinates(combined_sf), eps = 15, minPts = 500)$cluster)


plot(combined_sf$geometry, col = combined_sf$cluster)

cluster_colors <- rainbow(length(levels(combined_sf$cluster)))

leaflet() %>%
  addTiles() %>%
  setView(lng = -68.9000, lat = -32.8895, zoom = 13.5) %>%
  addCircleMarkers(data = st_transform(combined_sf, crs = 4326), 
                   color = ~cluster_colors[cluster],  # Color by cluster column
                   fillColor = ~cluster_colors[cluster],  # Fill color by cluster column
                   fillOpacity = 0.8,  # Set fill opacity
                   radius = 1)  # Set radius of the circle markers




# DBSCAN ~ rasters --------------------------------------------------------

# prueba polígono -> raster -> pointgrid

# rasterized <- rasterize(microbasurales,
#                         raster(ext = extent(microbasurales[x, ]), resolution = 0.5))
# 
# ncell(rasterized)
# 
# plot(rasterized, col = "red")

#

rasterized <- rasterize(microbasurales,
                        raster(ext = extent(microbasurales), resolution = 1))

ncell(rasterized)

plot(rasterized)

coordpoints <- st_as_sf(as.data.frame(rasterToPoints(rasterized)),
                        coords = c("x", "y"))

st_crs(coordpoints) <- st_crs(microbasurales)


# plot(coordpoints, col = "grey")
# 
# plot(microbasurales$geometry)

coordpoints$cluster <- as.factor(dbscan(st_coordinates(coordpoints),
                                        eps = 25,
                                        minPts = 80)$cluster)

coordpoints$cluster <- as.numeric(dbscan(st_coordinates(coordpoints),
                                        eps = 10,
                                        minPts = 50)$cluster)


cluster_colors <- rainbow(length(levels(coordpoints$cluster)))
cluster_colors[1] <- "grey"

ggplot() +
  geom_sf(data = fracc_cens_ciudad, col = "black", alpha = 0, size = 0.1) +
  geom_sf(data = coordpoints, size = 1, alpha = 0.5, aes(col = as.numeric(cluster))) +
  scale_color_viridis_c() +
  theme_minimal() +
  xlim(c(68.94, 68.88)) +
  ylim(c(32.91, 32.87)) +
  theme(legend.position = "none")




leaflet() %>%
  addTiles() %>%
  setView(lng = -68.9000, lat = -32.8895, zoom = 13.5) %>%
  addCircleMarkers(data = st_transform(coordpoints, crs = 4326), 
                   color = ~cluster_colors[cluster],  # Color by cluster column
                   fillColor = ~cluster_colors[cluster],  # Fill color by cluster column
                   fillOpacity = 0.3,  # Set fill opacity for transparency (0: fully transparent, 1: fully opaque)
                   opacity = 0.3, # Set border opacity for transparency (0: fully transparent, 1: fully opaque)
                   radius = 1)  # Set radius of the circle markers




