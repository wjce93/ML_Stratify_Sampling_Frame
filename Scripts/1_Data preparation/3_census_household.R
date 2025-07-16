rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Apertura de la base de datos de personas
censo_hog = import("./Data/CPV2010_Spss_Hogar.sav") %>%
  # rename_all(tolower) %>%
  # variables de ubicacion geografica
  select(I01, I02, I03, I04, I05, I09, I10, URH,
         # numero de cuartos o de dormitorios
         H01, H01N,
         # Instalaciones o si el hogar dispone de enseres
         H02, H03, H04, H05, H06, H07, H08, H09, H10, H11,
         # la vivienda es
         H15,
         # total de personas
         TP1, TH1, TM1
         ) %>%
  # renombrar algunas variables
  rename(prov = I01, cant = I02, parroq = I03, zona = I04, sector = I05, 
         vivienda = I09, hogar = I10, area = URH,
         h_num_cuartos_dormir = H01, h_ningun_dormitorio = H01N,
         h_cuarto_cocinar = H02, h_serv_higienico_es = H03, h_dispone_ducha = H04, 
         h_combustible_cocinar = H05, h_agua_tomar = H06, h_dispone_telef = H07, 
         h_dispone_celular = H08, h_dispone_internet = H09, h_dispone_compu = H10, 
         h_dispone_tvcable = H11,
         h_vivienda_es = H15,
         h_tot_per = TP1, h_tot_hom = TH1, h_tot_muj = TM1
         ) %>%
  rename_all(tolower) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(prov = str_pad(prov, 2, "left", "0"),
         cant = str_pad(cant, 2, "left", "0"),
         parroq = str_pad(parroq, 2, "left", "0"),
         zona = str_pad(zona, 3, "left", "0"),
         sector = str_pad(sector, 3, "left", "0"),
         vivienda = str_pad(vivienda, 3, "left", "0"),
         hogar = as.character(hogar)
         )

# Guardar la base de datos resultante
saveRDS(censo_hog, "./Data/CPV2010_Spss_Hogar.rds")
