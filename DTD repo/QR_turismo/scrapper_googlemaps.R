
# Scrapper Googlemaps

# buffers 2da seccion

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

## GOOGLEMAPS BD

places_list$lat <- places_list$geometry$location$lat
places_list$lng <- places_list$geometry$location$lng

places_list$types <- as.character(paste(places_list$types))

writexl::write_xlsx(places_list, path = "C:/Users/arysa/Downloads/places_list.xlsx")

# create sf
places_list_sf <- st_as_sf(places_list, coords = c("lng", "lat"), crs = st_crs(secciones))

places_list_sf <- places_list_sf %>% mutate(url_maps = paste0("https://www.google.com/maps/place/?q=place_id:", reference))

places_list_sf <- places_list_sf %>% filter(is.na(permanently_closed))

