
library(ckanr)
library(httr)
library(urltools)
library(tidyr)

ckanr_setup(url = "https://datos.ciudaddemendoza.gob.ar", key = "")

# check si el server está caído o corriendo
ping() # T = corriendo

# Información de la instancia
ckan_info()

# listas de elementos
organization_list()
group_list()

organization_show(id = "transformacion_digital", include_datasets = TRUE, as = "list")$packages
package_show(id = "12150e43-6c48-49b4-a29e-47a2518b45bc", include_datasets = TRUE, as = "list")$resources

# identificamos el recurso "acequias"
package_show("acequias")$resources[[1]] # 1 = csv / 2 = kmz

# importamos el recurso tal como está en ckan
library(readxl)
hidrografia_new <- read_excel("hidrografia_new.xlsx")
View(hidrografia_new)

# agregamos una columna para probar actualizarlo
hidrografia_new <- hidrografia_new %>% mutate(col_trial = "false")

# guardamos el archivo localmente (seguramente se puede guardar en un puerto temporal, pero parece que en algún lado debe estar guardada)
write.csv(hidrografia_new, file = paste0(getwd(), "/hidrografia_new.csv"), row.names = FALSE)

# actualizamos el recurso

ckan_url <- get_default_url()
api_key <- get_default_key()
resource_id <- package_show("acequias")$resources[[1]]$id
path <- paste0(getwd(), "/hidrografia_new.csv")

# Intento de actualizar el recurso con funciones naticas de ckanr
tryCatch({
  resource_update(
    id = resource_id,
    path = path,
    as = "list"
  )
  message("Resource updated successfully.")
}, error = function(e) {
  message("An error occurred: ", e$message)
})

# Intento de hacer un POST http
response <- POST(
  url = paste0(ckan_url, "/api/3/action/resource_update"),
  add_headers("Authorization" = api_key),
  body = list(
    id = resource_id,
    upload = upload_file(path)
  ),
  encode = "multipart"
)



## Alternativa

library(ckanr)
library(tidyr)
library(httr)
library(jsonlite)

# Replace these variables with your CKAN instance URL, API key, and dataset details
ckan_url <- "https://datos.ciudaddemendoza.gob.ar/api/3/action/package_create"
ckan_api_key <- ""

ckanr_setup(url = "https://datos.ciudaddemendoza.gob.ar", key = ckan_api_key)

# Replace with your dataset/package details
package_data <- toJSON(list(
  name = "metados_ckan",
  title = "Metadatos CKAN Ciudad de Mendoza",
  notes = "Metadatos extraídos de la instancia CKAN de la Municipalidad de la Ciudad de Mendoza",
  owner_org = "transformacion_digital"
  # Add more fields as needed (e.g., tags, resources)
))

# Create headers with authorization and content type
headers <- add_headers(
  Authorization = ckan_api_key,
  `Content-Type` = "application/json"
)

# Send POST request to create the package
response <- POST(url = ckan_url, body = package_data, headers = headers)

response$status_code


###



organization_show(id = "transformacion_digital", include_datasets = TRUE, as = "list")$id

organization_show(id = "transformacion_digital", include_datasets = TRUE, as = "table")$users %>% View()

package_create(name = "metados_ckan",
               title = "Metadatos CKAN Ciudad de Mendoza",
               private = F,
               author = "transformacion_digital",
               author_email = NULL,
               maintainer = NULL,
               maintainer_email = NULL,
               license_id = NULL,
               notes = NULL,
               package_url = NULL,
               version = NULL,
               state = "active",
               type = NULL,
               resources = NULL,
               tags = NULL,
               extras = NULL,
               relationships_as_object = NULL,
               relationships_as_subject = NULL,
               groups = NULL,
               owner_org = "transformacion_digital",
               url = get_default_url(),
               key = get_default_key(),
               as = "list")





