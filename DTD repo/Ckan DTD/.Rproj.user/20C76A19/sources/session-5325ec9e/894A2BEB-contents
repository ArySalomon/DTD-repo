

library(rvest)
library(httr)
library(tidyr)
library(openxlsx)

# Set the URL
page <- read_html("https://ciudaddemendoza.gob.ar/licitaciones-y-contrataciones/") # Licitaciones y contrataciones

page <- read_html("https://ciudaddemendoza.gob.ar/licitaciones-de-obras-publicas/") # Licitaciones de obra pública

# Function to clean and separate the text into 4 parts
clean_and_split <- function(text) {
  # Replace multiple newlines and tabs with a single newline
  text <- gsub("\n+", "\n", text)
  text <- gsub("\t+", "", text)
  
  # Split the text by newline
  parts <- unlist(strsplit(text, "\n"))
  
  # Trim whitespace and remove empty parts
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  
  return(parts)
}

# Initialize an empty list to store the elements
all_elements <- list()

# Loop over the range of XPaths
for (i in 4:290) {
  xpath <- paste0('//*[@id="page_container"]/div[3]/div/div/div[1]/div/div/div[', i, ']')
  
  element <- page %>% html_node(xpath = xpath) %>% html_text(trim = TRUE)
  
  if (!is.null(element)) {
    cleaned_element <- clean_and_split(element)
    all_elements[[length(all_elements) + 1]] <- cleaned_element
  }
}

# Convert the list to a dataframe
data_df <- do.call(rbind, all_elements) %>% as.data.frame(stringsAsFactors = FALSE)

data_df <- data_df %>% dplyr::mutate(V5 = ifelse(V5 == V1, NA, V5),
                                     V6 = ifelse(V6 == V1 | V6 == V2, NA, V6))

# Assign column names
colnames(data_df) <- c("Expediente", "Titulo", "Modalidad", "Detalle", "Aclaratoria1", "Aclaratoria2")


openxlsx::write.xlsx(data_df, file = paste0(getwd(), "/licitaciones_contrataciones_mza.xlsx"))


