

## GOOGLE MAPS SCRAPPER

# https://console.cloud.google.com/marketplace/product/google/places-backend.googleapis.com?q=search&referrer=search&hl=es-419&project=mapsapi-430314
# https://developers.google.com/maps/documentation/places/web-service/details?hl=es-419
# https://developers.google.com/maps/documentation/places/web-service/supported_types?hl=es-419

library(sf)
library(ggplot2)
library(leaflet)
library(writexl)

library(googleway)
library(tidyr)
library(purrr)


# Google maps APIkey
API_KEY <- ""

# Categorías seleccionadas para la consulta
places_types <- c(
  # "city_hall",
  # "local_government_office",
  # "embassy",
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
  # "light_rail_station",
  "locksmith",
  "painter",
  "pet_store",
  "plumber",
  "post_office",
  "real_estate_agency",
  "shoe_store",
  "storage",
  # "subway_station",
  # "taxi_stand",
  # "train_station",
  "veterinary_care",
  # "transit_station"
)

places_list <- lapply(places_types, function(x) list())
names(places_list) <- places_types

# Centroides seleccionados para la consulta
centroides <- list(c(-32.88350,-68.90119), # favorita
                  c(-32.88350,-68.84619)) # plaza chile

# Loop para consultar cada categoría en cada radio
for (centroide in seq_along(centroides)) { 
  for (type_n in seq_along(places_types)) { 
    
    # reporta inicio del proceso
    print(toupper(paste0("EXTRACCIÓN DE ", names(places_list)[type_n], " EN CENTROIDE ", centroide, " COMENZADA")))
    
    # consulta inicial
    new_places <- google_places(place_type = places_types[type_n], 
                                location = centroides[[centroide]],
                                radius = 4165, # radio de la consulta en metros
                                key = API_KEY)
    
    # reinicia el contador de iteraciones
    iter_numb <- 0
    
    # espera de 5 segundos para asegurar la integridad de las consultas
    Sys.sleep(5)
    
    # Whileloop para iterar por cada página de la respuesta (límite máximo de 20 filas)
    while (new_places$status == "OK" && nrow(data.frame(new_places$results)) > 0) { 
      
      # Guarda la categoría consultada en una columna de la respuesta
      new_places$results$requested_type <- places_types[[type_n]]
      
      # guarda la nueva consulta en la lista
      places_list[[type_n]][[length(places_list[[type_n]]) + 1]] <- data.frame(new_places$results) 
      
      # reporta el número de iteración
      iter_numb <- iter_numb + 1
      print(paste("ITERACIÓN NÚMERO: ", iter_numb)) 
      
      # reporta si el token de la siguiente página es nulo y rompe el whileloop
      if (is.null(new_places$next_page_token)) { 
        print("TOKEN NULO: NO HAY MÁS RESULTADOS")
        break
      }
      
      # nueva consulta con el token de la siguiente página
      new_places <- google_places(place_type = places_types[type_n], 
                                  location = centroides[[centroide]],
                                  radius = 4165,
                                  key = API_KEY,
                                  page_token = new_places$next_page_token)
      
      # espera de 5 segundos para asegurar la integridad de las consultas
      Sys.sleep(5) 
      
      # reporta si se rompe el whileloop
      if (new_places$status != "OK" || !nrow(data.frame(new_places$results)) > 0) {
        print("SOLICITUD INVÁLIDA O NO MÁS RESULTADOS")
      }
      
    }
    
    # reporta estado del proceso
    cat(toupper(paste0("EXTRACCIÓN DE ", names(places_list)[type_n], " EN CENTROIDE ", centroide, " FINALIZADA EN ", iter_numb, " ITERACIONES")), "\n\n")
    
  }
}

# Convierte la lista de resultados en una tabla
places_list <- places_list %>%
                unlist(recursive = FALSE) %>%
                dplyr::bind_rows() %>%
                dplyr::distinct(place_id, .keep_all = TRUE)

# Dataframe para guardar los detalles
details_dataframe <- data.frame(
  name = character(), 
  address = character(), 
  phone = character(), 
  intl_phone = character(), 
  lat = character(), 
  lng = character(), 
  weekday_text = character(), 
  place_id = character(), 
  rating = character(), 
  delivery = character(), 
  dine_in = character(), 
  reservable = character(), 
  serves_beer = character(), 
  serves_breakfast = character(), 
  serves_brunch = character(), 
  serves_dinner = character(), 
  serves_lunch = character(), 
  serves_vegetarian_food = character(), 
  serves_wine = character(), 
  takeout = character(), 
  wheelchair_accessible_entrance = character(), 
  user_ratings_total = character(), 
  website = character(), 
  url = character(), 
  types = character(),
  stringsAsFactors = FALSE
)


# Loop por cada id para solicitar los detalles

iter_numb <- 0

for (id in places_list$place_id) {
  
  # Guardamos los detalles
  details <- google_place_details(place_id = id,
                                  language = "es",
                                  key = API_KEY,
                                  simplify = TRUE)
  
  # Convertimos los detalles en una fila
  row <- data.frame(
    name = ifelse(!is.null(details$result$name),
                as.character(details$result$name),
                NA_character_),
    address = ifelse(!is.null(details$result$formatted_address),
                as.character(details$result$formatted_address),
                NA_character_),
    phone = ifelse(!is.null(details$result$formatted_phone_number),
                as.character(details$result$formatted_phone_number),
                NA_character_),
    intl_phone = ifelse(!is.null(details$result$international_phone_number),
                as.character(details$result$international_phone_number),
                NA_character_),
    lat = ifelse(!is.null(details$result$geometry$location$lat),
                 as.character(details$result$geometry$location$lat),
                 NA_character_),
    lng = ifelse(!is.null(details$result$geometry$location$lng),
                 as.character(details$result$geometry$location$lng),
                 NA_character_),
    weekday_text = ifelse(!is.null(details$result$opening_hours$weekday_text), 
                as.character(paste(details$result$opening_hours$weekday_text, collapse = ";;")), 
                NA_character_),
    place_id = ifelse(!is.null(details$result$place_id),
                as.character(details$result$place_id),
                NA_character_),
    rating = ifelse(!is.null(details$result$rating),
                as.character(details$result$rating),
                NA_character_),
    delivery = ifelse(!is.null(details$result$delivery), 
                as.character(paste(details$result$delivery)), 
                NA_character_),
    dine_in = ifelse(!is.null(details$result$dine_in), 
                as.character(paste(details$result$dine_in)), 
                NA_character_),
    reservable = ifelse(!is.null(details$result$reservable), 
                as.character(paste(details$result$reservable)), 
                NA_character_),
    serves_beer = ifelse(!is.null(details$result$serves_beer), 
                as.character(paste(details$result$serves_beer)), 
                NA_character_),
    serves_breakfast = ifelse(!is.null(details$result$serves_breakfast), 
                as.character(paste(details$result$serves_breakfast)), 
                NA_character_),
    serves_brunch = ifelse(!is.null(details$result$serves_brunch), 
                as.character(paste(details$result$serves_brunch)), 
                NA_character_),
    serves_dinner = ifelse(!is.null(details$result$serves_dinner), 
                as.character(paste(details$result$serves_dinner)), 
                NA_character_),
    serves_lunch = ifelse(!is.null(details$result$serves_lunch),
                as.character(paste(details$result$serves_lunch)), 
                NA_character_),
    serves_vegetarian_food = ifelse(!is.null(details$result$serves_vegetarian_food),
                as.character(paste(details$result$serves_vegetarian_food)), 
                NA_character_),
    serves_wine = ifelse(!is.null(details$result$serves_wine),
                as.character(paste(details$result$serves_wine)), 
                NA_character_),
    takeout = ifelse(!is.null(details$result$takeout),
                as.character(paste(details$result$takeout)), 
                NA_character_),
    wheelchair_accessible_entrance = ifelse(!is.null(details$result$wheelchair_accessible_entrance), 
                as.character(paste(details$result$wheelchair_accessible_entrance)), 
                NA_character_),
    user_ratings_total = ifelse(!is.null(details$result$user_ratings_total),
                as.character(details$result$user_ratings_total),
                NA_character_),
    website = ifelse(!is.null(details$result$website),
                as.character(details$result$website),
                NA_character_),
    url = ifelse(!is.null(details$result$url),
                as.character(details$result$url),
                NA_character_),
    stringsAsFactors = FALSE
  )
  
  # Guardamos la fila en el dataframe
  details_dataframe <- rbind(details_dataframe, row)
  
  # Reporte estado del proceso
  iter_numb <- iter_numb+1
  print(paste0("ITERACIÓN NÚMERO ", iter_numb))
  
}

# backup <- list(places_list, details_dataframe)

# Combinamos la tabla lugares con la tabla de detalles
places_details <- merge(details_dataframe[, c("place_id", colnames(details_dataframe)[-which(colnames(details_dataframe) %in% colnames(places_list))])], places_list, by = "place_id")

places_details <- places_details %>% dplyr::select(c(
                                    "name", "address", "phone", "intl_phone", "lat", "lng", "weekday_text", 
                                    "place_id", "rating", "delivery", "dine_in", "reservable", "serves_beer", 
                                    "serves_breakfast", "serves_brunch", "serves_dinner", "serves_lunch", 
                                    "serves_vegetarian_food", "serves_wine", "takeout", 
                                    "wheelchair_accessible_entrance", "user_ratings_total", "website", 
                                    "url", "types", "requested_type", "permanently_closed", "price_level"
                                  )) %>% dplyr::mutate(types = purrr::map_chr(types, ~ ifelse(!is.null(.x), paste(.x, collapse = ";;"), NA_character_)))


writexl::write_xlsx(places_details, path = "C:/Users/arysa/Downloads/places_details.xlsx")
save.image(file = "googlemaps_request.RData")


  # Alojamiento
  # Agencias de viaje
  # Gastronomia
  # Vinoteca
  # Alquileresde vehículos
  
 
  c(  # faltante
    # "city_hall",
    # "local_government_office",
    # "embassy",
    # "beauty_salon",
    # "clothing_store",
    # "electronics_store",
    # "fire_station",
    # "florist",
    # "furniture_store",
    # "gym",
    # "hair_care",
    # "hardware_store",
    # "home_goods_store",
    # "jewelry_store",
    # "library",
    # "light_rail_station",
    # "locksmith",
    # "painter",
    # "pet_store",
    # "plumber",
    # "post_office",
    # "real_estate_agency",
    # "shoe_store",
    # "storage",
    # "subway_station",
    # "taxi_stand",
    # "train_station",
    # "veterinary_care",
    # "transit_station"
                        ) %in%
  c( # ya solicitado
      "police",
      "car_dealer",
      "car_rental",
      "casino",
      "church",
      "supermarket",
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
      "lodging"
      )




