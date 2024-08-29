

save.image(file = "googlemaps_request1.RData")


# create sf

library(readxl)
places_list <- read_excel("places_list.xlsx")
View(places_list)

comercio_sf <- comercio %>% 
  mutate(segunda_sec = check_point_intersections(geompoints = ., buffers = secciones)) %>% 
  filter(segunda_sec == TRUE, is.na(fecha_baja_referencia_act))


manzanas_sf <- manzanas_sf %>% 
  mutate(segunda_sec = check_point_intersections(geompoints = ., buffers = secciones)) %>% 
  filter(segunda_sec == "TRUE")


parcelas <- parcelas %>%
  st_make_valid() %>%
  mutate(segunda_sec = check_point_intersections(geompoints = st_centroid(.), buffers = secciones)) %>%
  filter(segunda_sec == "TRUE")


parcelas$restringidas <- st_intersects(parcelas, locaciones_restringidas, sparse = FALSE) %>% apply(1, any)

st_intersects(parcelas, locaciones_restringidas, sparse = FALSE) %>% apply(1, any)

check_point_intersections()




propiedad_horizontal <- propiedad_horizontal %>%
  st_make_valid() %>%
  mutate(segunda_sec = check_point_intersections(geompoints = st_centroid(.), buffers = secciones)) %>%
  filter(segunda_sec == "TRUE")


espacios_verdes <- espacios_verdes %>%
  st_make_valid() %>%
  mutate(segunda_sec = check_point_intersections(geompoints = st_centroid(.), buffers = secciones)) %>%
  filter(segunda_sec == "TRUE")

locaciones_restringidas <- locaciones_restringidas %>% 
  mutate(segunda_sec = check_point_intersections(geompoints = ., buffers = secciones)) %>% 
  filter(segunda_sec == "TRUE")

# Arreglamos la capa "Gimnasios municipales" (polígonos)
# locaciones_restringidas <- st_centroid(locaciones_restringidas)

##


comercio_sf <- nearest_polygon(points_sf = comercio_sf,
                               polygons_sf = manzanas_sf,
                               polygon_id_col = "objectid_1")


locaciones_restringidas <- nearest_polygon(points_sf = locaciones_restringidas,
                                           polygons_sf = manzanas_sf,
                                           polygon_id_col = "objectid_1")





places_list_sf <- st_as_sf(places_list, coords = c("lng", "lat"), crs = st_crs(secciones)) %>% 
  mutate(url_maps = paste0("https://www.google.com/maps/place/?q=place_id:", reference)) %>% 
  filter(is.na(permanently_closed)) %>%
  mutate(segunda_sec = check_point_intersections(geompoints = ., buffers = secciones),
         espacio_verde = check_point_intersections(geompoints = ., buffers = espacios_verdes, radius = 2)) %>% # filter segunda seccion
  filter(segunda_sec == TRUE, !espacio_verde == "TRUE")


places_list_sf <- nearest_polygon(points_sf = places_list_sf,
                                  polygons_sf = manzanas_sf,
                                  polygon_id_col = "objectid_1") %>%
                                  filter(!nearest_polygon_id %in% c(605, 610)) # excluimos manzanas excepcionales





# calculamos el punto mínimo más cercano
places_list_sf$dist_comercio <- calculate_nearest_distance(basesf = places_list_sf, nearsf = comercio_sf, polygon_id_col = "nearest_polygon_id")
places_list_sf$dist_comercio[is.na(places_list_sf$dist_comercio)] <- max(places_list_sf$dist_comercio, na.rm = TRUE) # si hay NA le asignamos el max(dist_comercio)
places_list_sf$sin_cuenta <- ifelse(places_list_sf$dist_comercio > 10, TRUE, FALSE) # Variable categórica para clasificar los que están a + 10m de una cuenta NO HABILITADOS

# Columna para indicar si tiene una cuenta a 1 metros e indicar si es PH
places_list_sf$phorizontal <- check_point_intersections(geompoints = places_list_sf, buffers = propiedad_horizontal, radius = 1)

# Columna para indicar si tiene una locación restringida a 10 metros (solo se muestran ubicaciones de googlemaps gastronómicas)
places_list_sf$sociedad <- st_intersects(places_list_sf, parcelas %>% 
                                           dplyr::filter(restringidas == "TRUE"),sparse = FALSE) %>% apply(1, any)

places_list_sf <- places_list_sf %>% # solo los que interseccionan con sociedad y NO son gastronómicos, se marcan TRUE en "sociedad_clean"
  dplyr::mutate(
    gastronomico = str_detect(types, regex("food|restaurant|bar", ignore_case = TRUE)),
    sociedad_clean = ifelse(sociedad == "TRUE" & gastronomico == "FALSE", "TRUE",
                            ifelse(sociedad == "TRUE" & gastronomico == "TRUE", "FALSE", "FALSE")
    )
  )


## 

places_list_sf %>% # Places cercanos a un lugar restringido NO GASTRONOMICOS (en azul los que zafan de la clasificación en "sociedad_clean")
  dplyr::filter(sin_cuenta == "TRUE", sociedad_clean == "TRUE") %>% 
  dplyr::ungroup()


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = manzanas_sf,
    color = ~colorFactor(palette = "Set1", domain = manzanas_sf$objectid_1)(objectid_1),
    weight = 2,
    fillOpacity = 0.1
  ) %>%
  addPolygons(
    data = parcelas,
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









