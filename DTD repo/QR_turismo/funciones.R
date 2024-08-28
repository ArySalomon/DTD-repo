
library(tidyr)
library(leaflet)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(sf)
library(plotly)
library(RColorBrewer)
library(googleway)
library(log4r)



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



