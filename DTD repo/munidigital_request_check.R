
install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)


token_test <- ""
token <- ""
url_test <- "https://test-munidigital-core.munidigital.net/MuniDigitalCore/api"
url <- "https://munidigital.com/MuniDigitalCore/api"

# Function to get incidents
obtener_incidentes <- function(fecha_desde, fecha_hasta) {
  endpoint <- sprintf("%s/incidentes?fecha_desde=%s&fecha_hasta=%s", url, fecha_desde, fecha_hasta)
  
  headers <- add_headers(
    token = token,
    `Content-Type` = "application/json;charset=UTF-8"
  )
  
  response <- GET(endpoint, headers)
  
  if (status_code(response) == 200) {
    content(response, "text") %>% fromJSON() %>% .[["result"]]
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}

# Function to get messages
obtener_mensajes <- function(fecha_desde, fecha_hasta) {
  endpoint <- sprintf("%s/mensajes?fecha_desde=%s&fecha_hasta=%s", url, fecha_desde, fecha_hasta)
  
  headers <- add_headers(
    token = token,
    `Content-Type` = "application/json;charset=UTF-8"
  )
  
  response <- GET(endpoint, headers)
  
  if (status_code(response) == 200) {
    content(response, "text") %>% fromJSON() %>% .[["result"]]
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}

# Function to get citizens
obtener_ciudadanos <- function(fecha_desde, fecha_hasta) {
  endpoint <- sprintf("%s/ciudadanos?fecha_desde=%s&fecha_hasta=%s", url, fecha_desde, fecha_hasta)
  
  headers <- add_headers(
    token = token,
    `Content-Type` = "application/json;charset=UTF-8"
  )
  
  response <- GET(endpoint, headers)
  
  if (status_code(response) == 200) {
    content(response, "text") %>% fromJSON() %>% .[["result"]]
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}

# Function to get users
obtener_usuarios <- function() {
  endpoint <- sprintf("%s/usuarios", url)
  
  headers <- add_headers(
    token = token,
    `Content-Type` = "application/json;charset=UTF-8"
  )
  
  response <- GET(endpoint, headers)
  
  if (status_code(response) == 200) {
    content(response, "text") %>% fromJSON() %>% .[["result"]]
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}

# Function to get orders
obtener_ordenes <- function() {
  endpoint <- sprintf("%s/ordenes", url)
  
  headers <- add_headers(
    token = token,
    `Content-Type` = "application/json;charset=UTF-8"
  )
  
  response <- GET(endpoint, headers)
  
  if (status_code(response) == 200) {
    content(response, "text") %>% fromJSON() %>% .[["result"]]
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}

# Function to get incident states history
obtener_historial_estados <- function() {
  endpoint <- sprintf("%s/incidentes/historial-estados", url)
  
  headers <- add_headers(
    token = token,
    `Content-Type` = "application/json;charset=UTF-8"
  )
  
  response <- GET(endpoint, headers)
  
  if (status_code(response) == 200) {
    content(response, "text") %>% fromJSON() %>% .[["result"]]
  } else {
    stop("Request failed with status code: ", status_code(response))
  }
}


# Example usage of the functions
if (interactive()) {
  # Define date range
  fecha_desde <- "01/01/2023"
  fecha_hasta <- "06/30/2024"
  
  # Obtain incidents
  incidents <- obtener_incidentes(fecha_desde, fecha_hasta)
  print("Incidents:")
  print(incidents)
  
  # Obtain messages
  messages <- obtener_mensajes(fecha_desde, fecha_hasta)
  print("Messages:")
  print(messages)
  
  # Obtain citizens
  citizens <- obtener_ciudadanos(fecha_desde, fecha_hasta)
  print("Citizens:")
  print(citizens)
  
  # Obtain users
  users <- obtener_usuarios()
  print("Users:")
  print(users)
  
  # Obtain orders
  orders <- obtener_ordenes()
  print("Orders:")
  print(orders)
  
  # Obtain incident states history
  incident_states_history <- obtener_historial_estados()
  print("Incident States History:")
  print(incident_states_history)
}
