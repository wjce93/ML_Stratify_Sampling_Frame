rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Apertura de la base de datos de personas
censo_viv = import("./Data/CPV2010_Spss_Vivienda.sav") %>%
  # rename_all(tolower) %>%
  # variables de ubicacion geografica
  select(I01, I02, I03, I04, I05, I09, I10, URV,
         # tipo de vivienda
         VTV, VAP, VCO,
         # materiales de la vivienda
         V123, V01, V03, V05,
         # estado de la vivienda
         V02, V04, V06,
         # servicios basicos de la vivienda
         V07, V08, V09, V10, V11,
         # focos de la vivienda
         V12A, V12B,
         # eliminacion de basura
         V13,
         # otras variables importantes
         TOTPER, TOTDOR, PERCUA, PERDOR
         ) %>%
  # renombrar algunas variables
  rename(prov = I01, cant = I02, parroq = I03, zona = I04, sector =I05, 
         vivienda = I09, hogar = I10, area = URV,
         v_tipo_vivienda = VTV, v_via_acceso = VAP, v_condicion_ocu = VCO,
         v_materiales_viv = V123, v_material_techo = V01, v_material_paredes = V03, 
         v_material_piso = V05,
         v_estado_techo = V02, v_estado_paredes = V04, v_estado_piso = V06,
         v_lugar_proviene_agua = V07, v_agua_es = V08, v_serv_higienico_es = V09, 
         v_lugar_proviene_luz = V10, v_dispone_medidor = V11,
         v_focos_ahorrad = V12A, v_focos_convenc = V12B,
         v_eliminacion_basura = V13,
         v_total_pers = TOTPER, v_total_dorm = TOTDOR, 
         v_pers_cuarto = PERCUA, v_pers_dormit = PERDOR
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
saveRDS(censo_viv, "./Data/CPV2010_Spss_Vivienda.rds")
