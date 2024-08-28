
library(openxlsx)
library(ckanr)
library(httr)
library(urltools)
library(dplyr)
library(tidyr)
library(stringr)

# Setup CKAN connection
ckanr_setup(url = "https://datos.ciudaddemendoza.gob.ar", key = "")
ping() ## check si el server está caído o corriendo (T = corriendo)

# Function to retrieve all packages with pagination
get_all_packages <- function() {
  all_packages <- list()
  limit <- 100
  offset <- 0
  
  repeat {
    packages <- package_list(as = "table", limit = limit, offset = offset)
    if (length(packages) == 0) break
    all_packages <- c(all_packages, packages)
    offset <- offset + limit
  }
  
  return(all_packages)
}

# Function to convert "frecuencia de actualización" to legible format
convert_periodicity <- function(periodicity_code) {
  periodicity_code <- as.character(periodicity_code)
  if (str_detect(periodicity_code, "Y")) {
    number <- str_extract(periodicity_code, "\\d+")
    return(paste(number, "años"))
  } else if (str_detect(periodicity_code, "M")) {
    number <- str_extract(periodicity_code, "\\d+")
    return(paste(number, "meses"))
  } else if (str_detect(periodicity_code, "S")) {
    return("Continuamente")
  } else {
    return(NA)
  }
}

# Function to determine update status
classify_update_status <- function(periodicity, last_updated) {
  
  # Initialize a vector to store the periodicity in days
  periodicity_days <- numeric(length(periodicity))
  
  for (i in seq_along(periodicity)) {
    if (grepl("años", periodicity[i])) {
      periodicity_days[i] <- as.numeric(gsub("[^0-9]", "", periodicity[i])) * 365
    } else if (grepl("meses", periodicity[i])) {
      periodicity_days[i] <- as.numeric(gsub("[^0-9]", "", periodicity[i])) * 30
    } else if (grepl("Continuamente", periodicity[i])) {
      periodicity_days[i] <- 0
    }
  }
  
  # Calculate days since last update
  last_updated_days <- as.numeric(difftime(Sys.Date(), last_updated, units = "days"))
  
  # Determine status
  status <- ifelse(last_updated_days <= periodicity_days, "Actualizado", "Desactualizado")
  
  return(status)
}

# Initialize data frames to store packages and resources data
packages_data <- data.frame(
  package_groups = character(),
  package_name = character(),
  package_description = character(),
  package_id = character(),
  organization = character(),
  package_url = character(),
  package_responsible = character(),
  package_mantainer = character(),
  package_mantainer_mail = character(),
  last_updated = character(),
  act_periodicity = character(),
  difftime_lastupdate = numeric(),
  update_status = character(),
  stringsAsFactors = FALSE
)

resources_data <- data.frame(
  package_id = character(),
  resource_id = character(),
  resource_name = character(),
  resource_url = character(),
  resource_format = character(),
  stringsAsFactors = FALSE
)


## Loop through each package to get resource and package details
for (package_id in get_all_packages()) {
  package_info <- package_show(id = package_id, as = "table")
  
  # Extract package data
  packages_data <- rbind(packages_data, data.frame(
    package_groups = paste(package_info$groups$title, collapse = " - "),
    package_name = package_info$title,
    package_description = package_info$notes,
    package_id = package_info$id,
    organization = ifelse(is.null(package_info$organization), NA, package_info$organization$title),
    package_url = package_info$url,
    package_responsible = package_info$author,
    package_mantainer = package_info$maintainer,
    package_mantainer_mail = package_info$maintainer_email,
    last_updated = package_info$metadata_modified,
    act_periodicity = convert_periodicity(ifelse(any(package_info$extras$key == "accrualPeriodicity"), 
                                                 package_info$extras[package_info$extras$key == "accrualPeriodicity", "value"], 
                                                 NA)),
    difftime_lastupdate = as.numeric(difftime(Sys.Date(), package_info$metadata_modified)),
    update_status = classify_update_status(convert_periodicity(ifelse(any(package_info$extras$key == "accrualPeriodicity"), 
                                                                      package_info$extras[package_info$extras$key == "accrualPeriodicity", "value"], 
                                                                      NA)),
                                           package_info$metadata_modified),
    stringsAsFactors = FALSE
  ))
  
  # Loop through each package to extract resource data
  for (resource_n in 1:length(package_info$resources$id)) {
    resources_data <- rbind(resources_data, data.frame(
      package_id = package_info$id,
      resource_id = package_info$resources$id[resource_n],
      resource_name = package_info$resources$name[resource_n],
      resource_url = package_info$resources$url[resource_n],
      resource_format = package_info$resources$format[resource_n],
      stringsAsFactors = FALSE
    ))
  }
}



write.xlsx(resources_data, file = paste0(getwd(), "/resources_data.xlsx"))
write.xlsx(packages_data, file = paste0(getwd(), "/packages_data.xlsx"))











