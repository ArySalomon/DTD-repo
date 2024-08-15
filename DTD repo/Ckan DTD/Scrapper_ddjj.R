

library(RSelenium)
library(netstat)
library(wdman)
library(rvest)
library(httr)
library(tidyr)
library(openxlsx)



# Webscrapper

library(readxl)
Declaraciones_Juradas_Funcionarios <- read_excel("C:/Users/arysa/Downloads/Declaraciones Juradas Funcionarios.xlsx")
View(Declaraciones_Juradas_Funcionarios)

Declaraciones_Juradas_Funcionarios$new_url <- NA

# Start the Selenium server with Firefox
rD <- rsDriver(browser = "firefox", port = 5555L, verbose = FALSE)
remDr <- rD$client

remDr$open() ## NO CERRAR

# rD$server$stop()    # Stop the Selenium server

for (numb in 1:nrow(Declaraciones_Juradas_Funcionarios)) {
  
  remDr$navigate(Declaraciones_Juradas_Funcionarios[ numb, ]$url)  ## Con esto abierto hay que navegar la página que se quiere scrappear
  remDr$findElement(using = 'xpath', '/html/body/main/div/div[3]/div[1]/table/tbody/tr/td[2]/a/i')$clickElement()  #Le estamos diciendo: busca en el html el tag 'a' donde el atributo 'aria-label' = "Siguiente"
  
  Declaraciones_Juradas_Funcionarios[numb, ]$new_url <- remDr$getCurrentUrl()
  
}


## Http requests & workbook save - loop

rm(numb)

numb <- 1

for (numb in 1:nrow(Declaraciones_Juradas_Funcionarios)) {
  
  
  response <- content(GET(unlist(Declaraciones_Juradas_Funcionarios[ numb, ]$new_url)), "text")
  
  # Parse the HTML content
  response <- read_html(response)
  
  # Extract cards
  cards <- html_nodes(response, ".card")
  
  # Extract table headers
  table_headers <- html_nodes(response, "div.card-header strong") %>% html_text(trim = TRUE)
  
  # Initialize list to store tables with attributes
  list_of_tables <- list()
  
  rm(i, j)
  # Iterate through cards and extract tables
  for (i in seq_along(cards)) {
    card <- cards[[i]]
    
    # Extract table header
    header_text <- html_node(card, ".card-header strong") %>% html_text(trim = TRUE)
    
    # Extract tables within the card
    tables <- html_nodes(card, "table")
    
    # Process only if there are tables
    if (length(tables) > 0) {
      # Process each table
      for (j in seq_along(tables)) {
        table <- tables[[j]]
        
        # Extract the headers
        header <- table %>%
          html_nodes("thead th") %>%
          html_text(trim = TRUE)
        
        # Extract the rows
        rows <- table %>%
          html_nodes("tbody tr") %>%
          lapply(function(row) {
            html_nodes(row, "td") %>%
              html_text(trim = TRUE)
          }) %>%
          do.call(rbind, .) %>%
          as.data.frame(stringsAsFactors = FALSE)
        
        # Set the column names
        colnames(rows) <- header
        
        # Add the table header as an attribute, check if header_text exists in table_headers
        rows <- rows %>% dplyr::mutate(Tipo_informacion = table_headers[[i]])
        
        # Add to list_of_tables
        list_of_tables[[length(list_of_tables) + 1]] <- rows
        
      }
    }
  }
  
  rm(sheet)
  # Create a new workbook
  wb <- createWorkbook()
  
  # Iterate through the list of data frames and add each to a separate sheet
  for (sheet in seq_along(list_of_tables)) {
    
    sheet_name <- paste("Sheet", i, sep = "_")  # Generate a generic sheet name
    addWorksheet(wb, sheet_name)
    
    # Write the header text
    writeData(wb, sheet = sheet_name, x = unique(list_of_tables[[sheet]]$Tipo_informacion)[1], startCol = 1, startRow = 1, colNames = FALSE, rowNames = FALSE)
    
    # Write the data frame below the header
    writeData(wb, sheet = sheet_name, x = list_of_tables[[sheet]], startCol = 1, startRow = 2)
    
  }
  
  # Save the workbook to a file
  
  saveWorkbook(wb, file = paste0("C:/Users/arysa/Downloads/", paste0(Declaraciones_Juradas_Funcionarios$Nombre, " ", Declaraciones_Juradas_Funcionarios$Apellido)[numb], ".xlsx"),
               overwrite = TRUE)
  
  print(paste0("Completado ", round((numb/nrow(Declaraciones_Juradas_Funcionarios)*100), 2), "%"))
  
}


