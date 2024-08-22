
library(tidyr)
library(leaflet)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(sf)
library(plotly)
library(RColorBrewer)


get_arcgis_services <- function(service = NULL, folder = NULL, layer = NULL, return_geojson = FALSE) {
  # Define the root URL of the ArcGIS REST API services directory
  root_url <- "https://webgis.ciudaddemendoza.gob.ar/server/rest/services"
  
  # Get token
  token_response <- httr::POST("https://webgis.ciudaddemendoza.gob.ar/portal/sharing/rest/generateToken",
                               body = list(
                                 username = "cchavarini",
                                 password = "",
                                 referer = "webgis.ciudaddemendoza.gob.ar/portal",
                                 f = "json"),
                               encode = "form")
  
  token <- httr::content(token_response)$token
  
  if (!is.null(folder) & !is.null(service)) {
    # Construct URL for querying features if folder and service are provided
    service_url <- paste0(root_url, "/", folder, "/", service, "/FeatureServer/1/query")
    
    if (return_geojson) {
      # Request the features of the layer as GeoJSON
      resultOffset <- 0
      all_data <- list()
      params <- list(
        where = "1=1",
        outFields = "*",
        f = "geojson",
        token = token,
        resultOffset = resultOffset,
        resultRecordCount = 1000  # Adjust as necessary, the default might be 1000
      )
      
      repeat {
        # Make the HTTP request
        response <- GET(service_url, query = params)
        
        # Check if the request was successful
        if (status_code(response) != 200) {
          stop("Request failed with status code: ", status_code(response))
        }
        
        # Parse the response
        geojson <- content(response, "text", encoding = "UTF-8")
        temp_data <- st_read(geojson, quiet = TRUE)
        
        # Break if no more data is returned
        if (nrow(temp_data) == 0) {
          break
        }
        
        # Append the data to the list
        all_data <- append(all_data, list(temp_data))
        
        # Increment the resultOffset
        resultOffset <- resultOffset + nrow(temp_data)
        params$resultOffset <- resultOffset
      }
      
      # Function to add missing columns
      add_missing_columns <- function(df, all_columns) {
        missing_columns <- setdiff(all_columns, colnames(df))
        df[missing_columns] <- NA
        return(df)
      }
      
      # Get all unique columns
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      
      # Add missing columns to all dataframes
      all_data <- lapply(all_data, add_missing_columns, all_columns = all_columns)
      
      # Combine all the data into a single sf object
      geojson_layer <- do.call(rbind, all_data)
      
      return(geojson_layer)
    }
  } else if (!is.null(folder)) {
    # If only folder is provided, get the services within the folder
    folder_url <- paste0(root_url, "/", folder)
    response <- GET(folder_url, query = list(token = token, f = "json"))
    
    if (status_code(response) != 200) {
      stop("Request failed with status code: ", status_code(response))
    }
    
    services <- fromJSON(content(response, "text"))
    return(services)
  } else if (is.null(service)) {
    # If no 'service' and 'folder' arguments are provided, get the list of all services
    response <- GET(root_url, query = list(token = token, f = "json"))
    services <- fromJSON(content(response, "text"))
    return(services)
  } else if (is.null(layer)) {
    # If 'service' is provided but not 'layer', get the layers within that specific service
    service_url <- paste0(root_url, "/", service, "/MapServer")
    response <- GET(service_url, query = list(f = "json"))
    layers <- fromJSON(content(response, "text"))
    return(layers$layers)
  } else {
    # If both 'service' and 'layer' are provided
    layer_url <- paste0(root_url, "/", service, "/FeatureServer/", layer, "/query")
    
    if (return_geojson) {
      # Request the features of the layer as GeoJSON
      resultOffset <- 0
      all_data <- list()
      params <- list(
        where = "1=1",
        outFields = "*",
        f = "geojson",
        token = token,
        resultOffset = resultOffset,
        resultRecordCount = 1000  # Adjust as necessary, the default might be 1000
      )
      
      repeat {
        # Make the HTTP request
        response <- GET(layer_url, query = params)
        
        # Check if the request was successful
        if (status_code(response) != 200) {
          stop("Request failed with status code: ", status_code(response))
        }
        
        # Parse the response
        geojson <- content(response, "text", encoding = "UTF-8")
        temp_data <- st_read(geojson, quiet = TRUE)
        
        # Break if no more data is returned
        if (nrow(temp_data) == 0) {
          break
        }
        
        # Append the data to the list
        all_data <- append(all_data, list(temp_data))
        
        # Increment the resultOffset
        resultOffset <- resultOffset + nrow(temp_data)
        params$resultOffset <- resultOffset
      }
      
      # Function to add missing columns
      add_missing_columns <- function(df, all_columns) {
        missing_columns <- setdiff(all_columns, colnames(df))
        df[missing_columns] <- NA
        return(df)
      }
      
      # Get all unique columns
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      
      # Add missing columns to all dataframes
      all_data <- lapply(all_data, add_missing_columns, all_columns = all_columns)
      
      # Combine all the data into a single sf object
      geojson_layer <- do.call(rbind, all_data)
      
      return(geojson_layer)
    } else {
      # Request the layer details
      response <- GET(layer_url, query = list(f = "json", token = token))
      layer_details <- fromJSON(content(response, "text"))
      return(layer_details)
    }
  }
}

# manzanas (layer)
get_arcgis_services()
get_arcgis_services(service = "Catastro_público")
manzanas_sf <- get_arcgis_services(service = "Catastro_público", layer = 3, return_geojson = TRUE)

# Parcelas - PH (layer)
get_arcgis_services()
get_arcgis_services(service = "Catastro_público")

parcelas <- get_arcgis_services(service = "Catastro_público", layer = 1, return_geojson = TRUE)

propiedad_horizontal <- get_arcgis_services(service = "Catastro_público", layer = 7, return_geojson = TRUE)

# Espacios verdes (layer)
get_arcgis_services()
get_arcgis_services(service = "Ambiente_público")
espacios_verdes <- get_arcgis_services(service = "Ambiente_público", layer = 10, return_geojson = TRUE)

# comercios (folder)
get_arcgis_services(folder = "Comercio")
comercio <- get_arcgis_services(folder = "Comercio", service = "Comercio", return_geojson = TRUE)

# Sociedad
get_arcgis_services()
get_arcgis_services(service = "Sociedad_público")


get_arcgis_services(service = "Sociedad_público")[(c(4, 5, 6, 11, 12, 14, 16, 19, 24, 26, 27, 28, 29, 31, 33, 34, 35, 43)+1), ]$name
get_arcgis_services(service = "Sociedad_público")[-(c(4, 5, 6, 11, 12, 14, 16, 19, 24, 26, 27, 28, 29, 31, 33, 34, 35, 43)+1), ]$name


rm(locaciones_restringidas, x)


for (x in c(4, 5, 6, 11, 12, 14, 16, 19, 24, 26, 27, 28, 29, 31, 33, 34, 35, 43)) {
  
  sociedad_new <- get_arcgis_services(service = "Sociedad_público", layer = x, return_geojson = TRUE)
  
  if (x == 44) { # la capa 44 es la única que no tiene nombre. Entonces nombramos con domicilio
    sociedad_new <- sociedad_new[which(colnames(sociedad_new) == "domicilio")]
    colnames(sociedad_new) <- c("nombre", "geometry")
  } else {
    sociedad_new <- sociedad_new[which(colnames(sociedad_new) %in% c("nombre", "desc_full", "name", "clubes", "titular"))[1]]
    colnames(sociedad_new) <- c("nombre", "geometry")
  }
  
  sociedad_new$tipo <- get_arcgis_services(service = "Sociedad_público")[x+1, ]$name
  
  if (is.na(st_crs(sociedad_new))) {
    sociedad_new <- st_transform(sociedad_new, crs = 4326)
  }
  
  if (!exists("locaciones_restringidas")) {
    locaciones_restringidas <- sociedad_new
  } else {
    locaciones_restringidas <- rbind(locaciones_restringidas, sociedad_new)
  }
}


# Arreglamos la capa "Gimnasios municipales" (polígonos)
locaciones_restringidas <- st_centroid(locaciones_restringidas)


# Googlescrapper
# places_list$lat <- places_list$geometry$location$lat
# places_list$lng <- places_list$geometry$location$lng
# 
# places_list$types <- as.character(paste(places_list$types))
# 
# writexl::write_xlsx(places_list, path = "C:/Users/arysa/Downloads/places_list.xlsx")
# 

save.image(file = "googlemaps_request.RData")

# Función para identificar puntos dentro de un buffer
check_point_intersections <- function(buffers, geompoints, radius = NULL) {
  # If a radius is provided, create buffer-polygons
  if (!is.null(radius)) {
    shapefile1_buffers <- st_buffer(buffers, dist = radius)
  } else { # If not, assumes that the shapefiles already contain buffer-polygons
    shapefile1_buffers <- buffers
  }
  
  # Check if any point in geompoints intersects with the buffers
  intersections <- st_intersects(geompoints, shapefile1_buffers, sparse = FALSE)
  
  # Create a logical vector indicating if an intersection was found for each point
  intersects_logical <- apply(intersections, 1, any)
  
  return(intersects_logical)
}

# Filter the polygons that fit within segunda seccion

places_list_sf <- st_as_sf(places_list, coords = c("lng", "lat"), crs = st_crs(secciones))
places_list_sf <- places_list_sf %>% dplyr::mutate(url_maps = paste0("https://www.google.com/maps/place/?q=place_id:", reference))
places_list_sf <- places_list_sf %>% dplyr::filter(is.na(permanently_closed))

places_list_sf$segunda_sec <- check_point_intersections(geompoints = places_list_sf, buffers = secciones)
places_list_sf <- places_list_sf %>% dplyr::filter(segunda_sec == "TRUE")


comercio$segunda_sec <- check_point_intersections(geompoints = comercio, buffers = secciones)
comercio_sf <- comercio %>% dplyr::filter(segunda_sec == "TRUE") %>% 
  dplyr::filter(is.na(fecha_baja_referencia_act))


manzanas_sf$segunda_sec <- check_point_intersections(geompoints = manzanas_sf, buffers = secciones)
manzanas_sf <- manzanas_sf %>% dplyr::filter(segunda_sec == "TRUE")

parcelas$segunda_sec <- check_point_intersections(geompoints = st_centroid(st_make_valid(parcelas)), buffers = secciones)
parcelas <- parcelas %>% dplyr::filter(segunda_sec == "TRUE")

propiedad_horizontal$segunda_sec <- check_point_intersections(geompoints = st_centroid(propiedad_horizontal), buffers = secciones)
propiedad_horizontal <- propiedad_horizontal %>% dplyr::filter(segunda_sec == "TRUE")

espacios_verdes$segunda_sec <- check_point_intersections(geompoints = st_centroid(espacios_verdes), buffers = secciones)
espacios_verdes <- espacios_verdes %>% dplyr::filter(segunda_sec == "TRUE")

locaciones_restringidas$segunda_sec <- check_point_intersections(geompoints = locaciones_restringidas, buffers = secciones)
locaciones_restringidas <- locaciones_restringidas %>% dplyr::filter(segunda_sec == "TRUE")


# Funcion para identificar manzana más cercana a cada punto y moverlos al borde
nearest_polygon <- function(points_sf, polygons_sf, polygon_id_col) {
  # Find the nearest polygon for each point and assign the corresponding polygon ID
  points_sf$nearest_polygon_id <- polygons_sf[[polygon_id_col]][st_nearest_feature(points_sf, polygons_sf)]
  
  # Move the points to the border of the nearest polygon
  points_sf <- points_sf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Filter the corresponding polygon using the associated column (e.g., "objectid_1")
      corresponding_polygon = st_geometry(manzanas_sf %>%
                                            dplyr::filter(objectid_1 == nearest_polygon_id)),
      # Calculate the nearest point on the polygon's border
      nearest_point = st_nearest_points(geometry, corresponding_polygon) %>%
        st_cast("POINT") %>%
        .[2] # Select the point on the polygon border
    ) %>%
    dplyr::ungroup() %>%
    st_set_geometry("nearest_point")
  
  return(points_sf)
}

places_list_sf <- nearest_polygon(points_sf = places_list_sf,
                                  polygons_sf = manzanas_sf,
                                  polygon_id_col = "objectid_1")

places_list_sf$espacio_verde <- check_point_intersections(geompoints = places_list_sf, buffers = espacios_verdes, radius = 2)
places_list_sf <- places_list_sf %>% dplyr::filter(!espacio_verde == "TRUE") %>% 
  dplyr::filter(!nearest_polygon_id %in% c(605, 610))


comercio_sf <- nearest_polygon(points_sf = comercio_sf,
                               polygons_sf = manzanas_sf,
                               polygon_id_col = "objectid_1")


locaciones_restringidas <- nearest_polygon(points_sf = locaciones_restringidas,
                                           polygons_sf = manzanas_sf,
                                           polygon_id_col = "objectid_1")



# Funcion para calcular el punto más cercano
calculate_nearest_distance <- function(basesf, nearsf, polygon_id_col) {
  # Ensure the polygon_id_col exists in both shapefiles
  if (!(polygon_id_col %in% colnames(basesf) && polygon_id_col %in% colnames(nearsf))) {
    stop("The specified polygon_id_col does not exist in one or both shapefiles.")
  }
  
  # Initialize a vector to store distances
  nearest_distances <- vector("numeric", length = nrow(basesf))
  
  # Loop over each row in the first shapefile
  for (i in seq_len(nrow(basesf))) {
    current_polygon_id <- basesf[[polygon_id_col]][i]
    
    # Filter nearsf by the same polygon_id as the current row in basesf
    filtered_nearsf <- nearsf %>%
      dplyr::filter(.data[[polygon_id_col]] == current_polygon_id)
    
    # Calculate the minimum distance to the nearest point in the filtered nearsf
    if (nrow(filtered_nearsf) > 0) {
      nearest_distances[i] <- min(st_distance(st_geometry(basesf[i, ]), st_geometry(filtered_nearsf)), na.rm = TRUE)
    } else {
      nearest_distances[i] <- NA_real_  # No matching points found in nearsf
    }
  }
  
  # Return the vector of distances
  return(nearest_distances)
}

# calculamos el punto mínimo más cercano
places_list_sf$dist_comercio <- calculate_nearest_distance(basesf = places_list_sf, nearsf = comercio_sf, polygon_id_col = "nearest_polygon_id")
places_list_sf$dist_comercio[is.na(places_list_sf$dist_comercio)] <- max(places_list_sf$dist_comercio, na.rm = TRUE) # si hay NA le asignamos el max(dist_comercio)
places_list_sf$sin_cuenta <- ifelse(places_list_sf$dist_comercio > 10, TRUE, FALSE) # Variable categórica para clasificar los que están a + 10m de una cuenta NO HABILITADOS

# Columna para indicar si tiene una cuenta a 1 metros e indicar si es PH
places_list_sf$phorizontal <- check_point_intersections(geompoints = places_list_sf, buffers = propiedad_horizontal, radius = 1)

# Columna para indicar si tiene una locación restringida a 10 metros (solo se muestran ubicaciones de googlemaps gastronómicas)
places_list_sf$dist_sociedad <- calculate_nearest_distance(basesf = places_list_sf, nearsf = locaciones_restringidas, polygon_id_col = "nearest_polygon_id")
places_list_sf$dist_sociedad[is.na(places_list_sf$dist_sociedad)] <- max(places_list_sf$dist_sociedad, na.rm = TRUE) # si hay NA le asignamos el max(dist)
places_list_sf$sociedad <- ifelse(places_list_sf$dist_sociedad < 20, TRUE, FALSE)# Variable categórica para clasificar cercanos a lugar restringido


places_list_sf <- places_list_sf %>% # solo los que interseccionan con sociedad y NO son gastronómicos, se marcan TRUE en "sociedad_clean"
  dplyr::mutate(
    gastronomico = str_detect(types, regex("food|restaurant|bar", ignore_case = TRUE)),
    sociedad_clean = ifelse(sociedad == "TRUE" & gastronomico == "FALSE", "TRUE",
                      ifelse(sociedad == "TRUE" & gastronomico == "TRUE", "FALSE", "FALSE")
    )
  )



leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = manzanas_sf,
    color = ~colorFactor(palette = "Set1", domain = manzanas_sf$objectid_1)(objectid_1),
    weight = 2,
    fillOpacity = 0.1
  ) %>%
  addPolygons(
    data = propiedad_horizontal,
    color = "grey",
    weight = 2,
    fillOpacity = 0
  ) %>%
  addCircleMarkers(
    data = places_list_sf %>% # Places cercanos a un lugar restringido NO GASTRONOMICOS (en azul los que zafan de la clasificación en "sociedad_clean")
      dplyr::filter(user_ratings_total > 3) %>%
      dplyr::rowwise() %>%
      dplyr::filter(
        !(any(str_detect(types, regex("tourist_attraction|park|local_government_office|museum|school|church|place_of_worship", ignore_case = TRUE))) &
            !any(str_detect(types, regex("store", ignore_case = TRUE))))
      ) %>%
      dplyr::filter(sin_cuenta == "TRUE", sociedad_clean == "TRUE") %>% 
      dplyr::ungroup(),
    radius = 5,
    color = "blue",
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste0("<strong>", name, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>")
  ) %>%
  addCircleMarkers(
    data = comercio_sf,
    radius = 5,
    color = "grey",
    fillOpacity = 0.1,
    stroke = FALSE,
    popup = ~nombre_fantasia
  ) %>% 
# %>%
#   addCircleMarkers(
#     data = places_list_sf %>% # Places cercanos a una cuenta comercial
#       dplyr::filter(user_ratings_total > 3) %>%
#       dplyr::rowwise() %>%
#       dplyr::filter(
#         !(any(str_detect(types, regex("tourist_attraction|park|local_government_office|museum|school|church|place_of_worship", ignore_case = TRUE))) &
#             !any(str_detect(types, regex("store", ignore_case = TRUE))))
#       ) %>%
#       dplyr::filter(sin_cuenta == "TRUE") %>% 
#       dplyr::ungroup(),
#     radius = 5,
#     color = ~"green",  # Conditional color based on 'intersect'
#     fillOpacity = 1,
#     stroke = FALSE,
#     popup = ~paste0("<strong>", name, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>")
#   )
# %>%
#   addCircleMarkers(
#     data = places_list_sf %>% # Places lejanos a una cuenta comercial
#       dplyr::filter(user_ratings_total > 3) %>%
#       dplyr::rowwise() %>%
#       dplyr::filter(
#         !(any(str_detect(types, regex("tourist_attraction|park|local_government_office|museum|school|church|place_of_worship", ignore_case = TRUE))) &
#             !any(str_detect(types, regex("store", ignore_case = TRUE))))
#       ) %>%
#       dplyr::filter(sin_cuenta == "FALSE", sociedad == "TRUE") %>% 
#       dplyr::ungroup(),
#     radius = 5,
#     color = ~colorNumeric(palette = c("gold", "orange", "red", "brown"), domain = places_list_sf$dist)(dist),  # Conditional color based on 'intersect'
#     fillOpacity = 1,
#     stroke = FALSE,
#     popup = ~paste0("<strong>", name, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>")
#   ) %>%
  addCircleMarkers(
    data = locaciones_restringidas,
    radius = 5,
    color = "magenta",
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste0(tipo, ": ", nombre)
  ) %>%
  addCircleMarkers(
    data = comercio_sf,
    radius = 5,
    color = "grey",
    fillOpacity = 0.1,
    stroke = FALSE,
    popup = ~nombre_fantasia
  )



leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = manzanas_sf,
    color = ~colorFactor(palette = "Set1", domain = manzanas_sf$objectid_1)(objectid_1),
    weight = 2,
    fillOpacity = 0.1
  ) %>%
  addPolygons(
    data = propiedad_horizontal,
    color = "grey",
    weight = 2,
    fillOpacity = 0
  ) %>%
    addCircleMarkers(
      data = places_list_sf %>% # Places cercanos a una cuenta comercial
        dplyr::filter(user_ratings_total > 3) %>%
        dplyr::rowwise() %>%
        dplyr::filter(
          !(any(str_detect(types, regex("tourist_attraction|park|local_government_office|museum|school|church|place_of_worship", ignore_case = TRUE))) &
              !any(str_detect(types, regex("store", ignore_case = TRUE))))
        ) %>%
        dplyr::filter(sin_cuenta == "FALSE") %>%
        dplyr::ungroup(),
      radius = 5,
      color = ~"green",  # Conditional color based on 'intersect'
      fillOpacity = 1,
      stroke = FALSE,
      popup = ~paste0("<strong>", name, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>")
    ) %>%
    addCircleMarkers(
      data = places_list_sf %>% # Places lejanos a una cuenta comercial
        dplyr::filter(user_ratings_total > 3) %>%
        dplyr::rowwise() %>%
        dplyr::filter(
          !(any(str_detect(types, regex("tourist_attraction|park|local_government_office|museum|school|church|place_of_worship", ignore_case = TRUE))) &
              !any(str_detect(types, regex("store", ignore_case = TRUE))))
        ) %>%
        dplyr::filter(sin_cuenta == "TRUE", sociedad_clean == "FALSE") %>% # probables inhabilitados solo los sin_cuenta y los que NO interseccionan con sociedad (o son gastronomicos)
        dplyr::ungroup(),
      radius = 5,
      color = ~colorNumeric(palette = c("gold", "orange", "red", "brown"), domain = places_list_sf$dist)(dist),  # Conditional color based on 'intersect'
      fillOpacity = 1,
      stroke = FALSE,
      popup = ~paste0("<strong>", name, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>")
    ) %>%
  addCircleMarkers(
    data = places_list_sf %>% # Places lejanos a una cuenta comercial
      dplyr::filter(user_ratings_total > 3) %>%
      dplyr::rowwise() %>%
      dplyr::filter(
        !(any(str_detect(types, regex("tourist_attraction|park|local_government_office|museum|school|church|place_of_worship", ignore_case = TRUE))) &
            !any(str_detect(types, regex("store", ignore_case = TRUE))))
      ) %>%
      dplyr::filter(sin_cuenta == "TRUE", sociedad_clean == "TRUE") %>% # excluidos porque interseccionan con sociedad y NO son gastronómicos (sirve de nada aparentemente)
      dplyr::ungroup(),
    radius = 5,
    color = "magenta",
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~paste0("<strong>", name, "</strong><br><a href='", url_maps, "' target='_blank'>", url_maps, "</a>")
  )


