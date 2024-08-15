
library(gsheet)
library(httr)
library(jsonlite)
library(sf)
library(tidyr)
library(leaflet)
library(stringr)
library(googleway)
library(purrr)
library(log4r)


# Define the root URL of the ArcGIS REST API services directory
# Send a GET request to the root URL
response <- GET("https://webgis.ciudaddemendoza.gob.ar/server/rest/services", query = list(f = "json"))

# Parse the JSON response
services <- fromJSON(content(response, "text"))

# Print the list of services
print(services$services)

# Service
service_url <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services/Demarcación_y_Divisiones/FeatureServer"

# Send a GET request to the FeatureServer URL
response <- GET(paste0(service_url, "?f=json"))

# Parse the JSON response
layer_info <- fromJSON(content(response, "text"))

# Print the layers and tables
print(layer_info$layers)

# Service Layers
service_url <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services/Demarcación_y_Divisiones/FeatureServer/6/query"

# Send a GET request to the FeatureServer URL
response <- GET(service_url, query = list(where = "1=1", outFields = "*", f = "geojson"))
geojson <- content(response, "text")
secciones <- st_read(geojson, quiet = TRUE) %>% dplyr::filter(seccion == "2")


rm(layer_info, response, service_url, services, geojson)


## BUFFERS SEGUNDA SECCION

# Define a function to create a grid of points within the polygon
create_grid_points <- function(polygon, spacing) {
  # Create a grid of points based on the polygon's bounding box
  bbox <- st_bbox(polygon)
  x_seq <- seq(bbox["xmin"], bbox["xmax"], by = spacing)
  y_seq <- seq(bbox["ymin"], bbox["ymax"], by = spacing)
  grid_points <- expand.grid(x = x_seq, y = y_seq)
  
  # Convert the grid points to an sf object
  grid_points_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = st_crs(polygon))
  
  # Keep only points that are within the polygon
  grid_points_within_polygon <- grid_points_sf[st_within(grid_points_sf, polygon, sparse = FALSE), ]
  
  return(grid_points_within_polygon)
}

# Create the grid points within the polygon
secciones_gid <- create_grid_points(secciones, spacing = 0.005)

secciones_gid$buffers <- st_buffer(secciones_gid$geometry, dist = 450)

leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%  # Detailed tile layer
  setView(lng = -68.87369, lat = -32.8835, zoom = 10) %>%  # Adjust zoom level here
  addPolygons(data = secciones, color = "#722F45", weight = 2, fillOpacity = 0.3) %>% 
  addPolygons(data = secciones_gid$buffers, color = "black", weight = 2, fillOpacity = 0) %>% 
  addCircleMarkers(data = secciones_gid,
                   radius = 5, # Adjust radius as needed
                   color = "red", # Customize color
                   fillOpacity = 0.5,
                   stroke = FALSE,
                   popup = ~as.character(geometry),
                   popupOptions = popupOptions(closeButton = TRUE, autoClose = FALSE, closeOnClick = TRUE))


rm(grid_points_sf, create_grid_points)


## SCRAPPER

log_folder <- "C:/Users/arysa/OneDrive/Github/DTD/QR_turismo/log_mapsapi"
if (!dir.exists(log_folder)) {
  dir.create(log_folder, recursive = TRUE)
}

log_file_path <- file.path(log_folder, paste0("log_", Sys.Date(), ".log"))
logger <- create.logger()
logfile(logger) <- log_file_path
level(logger) <- "INFO"

API_KEY <- ""

places_types <- c( # ya solicitado
  "police",
  "car_dealer",
  "car_rental",
  "casino",
  "church",
  "supermarket",
  # "city_hall",
  # "local_government_office",
  # "embassy",
  # "light_rail_station",
  # "subway_station",
  # "taxi_stand",
  # "train_station",
  # "transit_station",
  "tourist_attraction",
  "shopping_mall",
  "spa",
  "stadium",
  "pharmacy",
  "movie_theater",
  "museum",
  "night_club",
  "park",
  "parking",
  "liquor_store",
  "hospital",
  "doctor",
  "dentist",
  "book_store",
  "bank",
  "gas_station",
  "art_gallery",
  "travel_agency",
  "convenience_store",
  "bakery",
  "bar",
  "cafe",
  "meal_delivery",
  "meal_takeaway",
  "restaurant",
  "store",
  "department_store",
  "atm",
  "drugstore",
  "laundry",
  "insurance_agency",
  "lodging",
  "beauty_salon",
  "clothing_store",
  "electronics_store",
  "fire_station",
  "florist",
  "furniture_store",
  "gym",
  "hair_care",
  "hardware_store",
  "home_goods_store",
  "jewelry_store",
  "library",
  "locksmith",
  "painter",
  "pet_store",
  "plumber",
  "post_office",
  "real_estate_agency",
  "shoe_store",
  "storage",
  "veterinary_care"
)

places_list <- lapply(places_types, function(x) list())
names(places_list) <- places_types

centroides <- list(c(-32.89953,-68.84806),
                   c(-32.89953,-68.84306),
                   c(-32.89953,-68.83806),
                   c(-32.89453,-68.84806),
                   c(-32.89453,-68.84306),
                   c(-32.8895310026907,-68.848060851899),
                   c(-32.8895310026907,-68.843060851899))

info(logger, "Starting the extraction process")

for (centroide in seq_along(centroides)) {
  for (type_n in seq_along(places_types)) {
    
    # reporta inicio del proceso
    print(toupper(paste0("EXTRACCIÓN DE ", names(places_list)[type_n], " EN CENTROIDE ", centroide, " COMENZADA")))
    
    info(logger, paste("Extraction of", names(places_list)[type_n], "in centroid", centroide, "started"))
    
    new_places <- tryCatch({
      google_places(place_type = places_types[type_n],
                    location = centroides[[centroide]],
                    radius = 450,
                    key = API_KEY)
    }, error = function(e) {
      error(logger, paste("Error in PLACES API (first request):", e$message))
      NULL
    })
    
    if (is.null(new_places)) next
    
    iter_numb <- 0
    Sys.sleep(4)
    
    while (new_places$status == "OK" && nrow(data.frame(new_places$results)) > 0) {
      
      new_places$results$requested_type <- places_types[[type_n]]
      places_list[[type_n]][[length(places_list[[type_n]]) + 1]] <- data.frame(new_places$results)
      
      iter_numb <- iter_numb + 1
      info(logger, paste("Iteration number:", iter_numb))
      
      # reporta el número de iteración
      print(paste("ITERACIÓN NÚMERO: ", iter_numb)) 
      
      
      if (is.null(new_places$next_page_token)) {
        
        # reporta si el token de la siguiente página es nulo y rompe el whileloop
        print("TOKEN NULO: NO HAY MÁS RESULTADOS")
        
        info(logger, "Token null: No more results")
        break
      }
      
      new_places <- tryCatch({
        google_places(place_type = places_types[type_n],
                      location = centroides[[centroide]],
                      radius = 450,
                      key = API_KEY,
                      page_token = new_places$next_page_token)
      }, error = function(e) {
        error(logger, paste("Error in PLACES API (next page):", e$message))
        NULL
      })
      
      if (is.null(new_places)) break
      
      Sys.sleep(4)
      
      if (new_places$status != "OK" || !nrow(data.frame(new_places$results)) > 0) {
        
        # reporta si se rompe el whileloop
        print("SOLICITUD INVÁLIDA O NO MÁS RESULTADOS")
        
        info(logger, "Invalid request or no more results")
      }
      
    }
    
    # reporta estado del proceso
    cat(toupper(paste0("EXTRACCIÓN DE ", names(places_list)[type_n], " EN CENTROIDE ", centroide, " FINALIZADA EN ", iter_numb, " ITERACIONES")), "\n\n")
    
    info(logger, paste("Extraction of", names(places_list)[type_n], "in centroid", centroide, "completed in", iter_numb, "iterations"))
    
  }
}

places_list <- tryCatch({
  places_list %>%
    unlist(recursive = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(place_id, .keep_all = TRUE)
}, error = function(e) {
  error(logger, paste("Error in processing places_list:", e$message))
  data.frame()
})


save.image(file = "googlemaps_request.RData")


# Turismo service Arcgis -------------------------------------------------

vinoteca_service <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services/Turismo/Servicios_Turismo/FeatureServer/3/query"

gastronomia_service <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services/Turismo/Servicios_Turismo/FeatureServer/1/query"


token <- httr::content(httr::POST("https://webgis.ciudaddemendoza.gob.ar/portal/sharing/rest/generateToken",
                                  body = list(
                                    username = "cchavarini",
                                    password = "",
                                    referer = "webgis.ciudaddemendoza.gob.ar/portal",
                                    f = "json"),
                                  encode = "form"))$token
params <- list(
  where = "1=1",
  outFields = "*",
  returnGeometry = "true",
  f = "geojson",
  token = token
)

resultOffset <- 0
all_data <- list()

rm(combined_data)

repeat {
  # Update the resultOffset parameter
  params$resultOffset <- resultOffset
  
  # Make the HTTP request
  response <- GET(vinoteca_service, query = params)
  
  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("Request failed with status code: ", status_code(response))
  }
  
  # Parse the response
  geojson <- content(response, "text")
  temp_data <- st_read(geojson, quiet = TRUE)
  
  # Break if no more data is returned
  if (nrow(temp_data) == 0) {
    break
  }
  
  # Append the data to the list
  all_data <- append(all_data, list(temp_data))
  
  # Increment the resultOffset
  resultOffset <- resultOffset + nrow(temp_data)
}


# Función que compatibiliza las columnas en toda la lista
add_missing_columns <- function(df, all_columns) {
  missing_columns <- setdiff(all_columns, colnames(df))
  df[missing_columns] <- NA
  return(df)
}

all_columns <- unique(unlist(lapply(all_data, colnames)))

all_data <- lapply(all_data, add_missing_columns, all_columns = all_columns)

# Combine all the data into a single sf object
vinoteca_service <- do.call(rbind, all_data)

vinoteca_service <- st_join(vinoteca_service, secciones, join = st_within)
vinoteca_service <- vinoteca_service[!is.na(vinoteca_service$seccion), ]

#
gastronomia_service <- do.call(rbind, all_data)

gastronomia_service <- st_join(gastronomia_service, secciones, join = st_within)
gastronomia_service <- gastronomia_service[!is.na(gastronomia_service$seccion), ]

str(gastronomia_service)


####
## GOOGLEMAPS BD

places_list$lat <- places_list$geometry$location$lat
places_list$lng <- places_list$geometry$location$lng

places_list$types <- as.character(paste(places_list$types))


writexl::write_xlsx(places_list, path = "C:/Users/arysa/Downloads/places_list.xlsx")

# create sf
places_list_sf <- st_as_sf(places_list, coords = c("lng", "lat"), crs = st_crs(secciones))

places_list_sf <- places_list_sf %>% mutate(url_maps = paste0("https://www.google.com/maps/place/?q=place_id:", reference))

places_list_sf <- places_list_sf %>% filter(is.na(permanently_closed))

# filter segunda seccion
places_list_sf <- st_join(places_list_sf, secciones, join = st_within)
places_list_sf <- places_list_sf[!is.na(places_list_sf$seccion), ]

# CUAL ES EL MEJOR CRITERIO PARA FILTRAR?

# Acá podría ser 1 puntos si aparece acá, un punto más si no tiene una vinoteca habilitada a 50m,
# un punto más si no tiene un local gastronómico habilitado a 30m, un punto más si tiene en el nombre wine/vino/tinto
# Quizá un punto menos si tiene un local en proceso de habilitación a 50m
# Quizá un punto menos si tiene una cuenta en proceso a 50m
places_list_sf %>%
  filter(
    str_detect(name.x, regex("vino|wine|tinto|vine|vini", ignore_case = TRUE))
    &
      !str_detect(types, regex("restaurant|grocery_or_supermarket|cafe|convenience_store|book_store|bar", ignore_case = TRUE))
    & 
      str_detect(types, regex("liquor_store", ignore_case = TRUE))
  ),
) %>% View()

# Create a 50-meter buffer around target restaurants (google maps)
target_buffer <- st_buffer(places_list_sf %>%
                             filter(
                               str_detect(name.x, regex("vino|wine|tinto|vine|vini", ignore_case = TRUE))
                               &
                               !str_detect(types, regex("restaurant|grocery_or_supermarket|cafe|convenience_store|book_store|bar", ignore_case = TRUE))
                               & 
                                 str_detect(types, regex("liquor_store", ignore_case = TRUE))
                             ),
                           dist = 50)

# Find intersections
target_buffer <- st_intersects(gastronomia_service, target_buffer)

# Create a logical vector indicating which official restaurants are within any buffer
within_buffer <- sapply(target_buffer, function(x) length(x) > 0)

# Filter official restaurants
within_buffer <- gastronomia_service[within_buffer, ]

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # Minimalistic basemap
  addPolygons(data = secciones, color = "#722F45", weight = 2, fillOpacity = 0.3) %>% 
  addCircleMarkers(
    data = 
    places_list_sf %>%
      filter(
        str_detect(name.x, regex("vino|wine|tinto|vine|vini", ignore_case = TRUE))
        &
          !str_detect(types, regex("restaurant|grocery_or_supermarket|cafe|convenience_store|book_store|bar", ignore_case = TRUE))
        & 
          str_detect(types, regex("liquor_store", ignore_case = TRUE))
      ),
    radius = 5,
    color = "red",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~paste0("<strong>", name.x, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>") # Name and clickable hyperlink
  ) %>% 
  addCircleMarkers(
    data = vinoteca_service,
    radius = 5,
    color = "blue",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~nombre_fantasia # Name and clickable hyperlink
  ) %>%
  addCircleMarkers(
    data = within_buffer,
    radius = 5,
    color = "green",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~paste0(nombre_fantasia, " - ", desc_full) # Popup with 'nombre_fantasia' field
  )
