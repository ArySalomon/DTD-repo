
library(ggrepel)
library(gsheet)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(writexl)
library(xlsx)
library(googlesheets4)
library(lubridate)
library(viridis)

library(readxl)
govtech <- read_excel("C:/Users/arysa/Downloads/govtech.xlsx")
View(govtech)


colnames(govtech) <- c(colnames(govtech)[1:5], "rubro",
                       "hizo_tramite_comercial", "motivo_no_tramite",
                       "esta_habilitado", "motivo_no_habilitado",
                       "tuvo_dificultad", "describir_dificultad",
                       "que_consulta_previa", "que_medio_consulta", "indicar_medio_otro",
                       "cuantas_consultas", "cual_plataforma",
                       "necesito_conocer_normativa", "necesito_especialista",
                       "leyo_normativa_usos", "que_sintio_dificil",
                       "otro_parecio_dificil", "que_datos_utiles",
                       "que_visualizacion_mejor", "id", "uuid")




dificultad_long <- separate(govtech, tuvo_dificultad, into = c("dif_1", "dif_2", "dif_3", "dif_4", "dif_5", "dif_6",
                                                            "dif_7", "dif_8", "dif_9", "dif_10"), sep = " - ") %>% 
  select("id", "dif_1", "dif_2", "dif_3", "dif_4", "dif_5", "dif_6", "dif_7", "dif_8", "dif_9", "dif_10") %>% 
  pivot_longer(cols = c(2:11), names_to = "dificultad_nro", values_to = "dificultad") %>%
  filter(!is.na(dificultad))
  
table(dificultad_long$dificultad)



que_consulta_long <- separate(govtech, que_consulta_previa, into = c("qc_1", "qc_2", "qc_3", "qc_4", "qc_5", "qc_6",
                                                               "qc_7", "qc_8", "qc_9", "qc_10"), sep = " - ") %>% 
  select("id", "qc_1", "qc_2", "qc_3", "qc_4", "qc_5", "qc_6", "qc_7", "qc_8", "qc_9", "qc_10") %>% 
  pivot_longer(cols = c(2:11), names_to = "consulta_nro", values_to = "consulta") %>%
  filter(!is.na(consulta))

table(que_consulta_long$consulta)



que_medio_consulta_long <- separate(govtech, que_medio_consulta, into = c("qmc_1", "qmc_2", "qmc_3", "qmc_4", "qmc_5", "qmc_6",
                                                                     "qmc_7", "qmc_8", "qmc_9", "qmc_10"), sep = " - ") %>% 
  select("id", "qmc_1", "qmc_2", "qmc_3", "qmc_4", "qmc_5", "qmc_6", "qmc_7", "qmc_8", "qmc_9", "qmc_10") %>% 
  pivot_longer(cols = c(2:11), names_to = "medio_consulta_nro", values_to = "medio_consulta") %>%
  filter(!is.na("medio_consulta"))

table(que_medio_consulta_long$medio_consulta)



cual_plataforma_long <- separate(govtech, cual_plataforma, into = c("p_1", "p_2", "p_3", "p_4", "p_5", "p_6",
                                                                          "p_7", "p_8", "p_9", "p_10"), sep = " - ") %>% 
  select("id", "p_1", "p_2", "p_3", "p_4", "p_5", "p_6", "p_7", "p_8", "p_9", "p_10") %>% 
  pivot_longer(cols = c(2:11), names_to = "plataforma_nro", values_to = "plataforma") %>%
  filter(!is.na("plataforma"))

table(cual_plataforma_long$plataforma)



que_datos_utiles <- separate(govtech, que_datos_utiles, into = c("d_1", "d_2", "d_3", "d_4", "d_5", "d_6",
                                                                    "d_7", "d_8", "d_9", "d_10"), sep = " - ") %>% 
  select("id", "d_1", "d_2", "d_3", "d_4", "d_5", "d_6", "d_7", "d_8", "d_9", "d_10") %>% 
  pivot_longer(cols = c(2:11), names_to = "datos_nro", values_to = "datos") %>%
  filter(!is.na("datos"))

table(que_datos_utiles$datos)



write.xlsx(dificultad_long, paste0(getwd(), "/dificultad_long.xlsx"))
write.xlsx(que_consulta_long, paste0(getwd(), "/que_consulta_long.xlsx"))
write.xlsx(que_medio_consulta_long, paste0(getwd(), "/que_medio_consulta_long.xlsx"))
write.xlsx(cual_plataforma_long, paste0(getwd(), "/cual_plataforma_long.xlsx"))
write.xlsx(que_datos_utiles, paste0(getwd(), "/que_datos_utiles.xlsx"))

write.xlsx(govtech, paste0(getwd(), "/govtech_kobo.xlsx"))


