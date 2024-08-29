
# Segunda sección
get_arcgis_services()
get_arcgis_services(service = "Demarcación_y_Divisiones")
secciones <- get_arcgis_services(service = "Demarcación_y_Divisiones", layer = 6, return_geojson = TRUE) %>% dplyr::filter(seccion == "2")

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


