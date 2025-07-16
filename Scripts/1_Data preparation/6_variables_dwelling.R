
rm(list = ls())

library(rio)
library(tidyverse)
library(dplyr)

# Apertura de las bases de datos de vivienda
censo_viv = readRDS("./Data preparation/1_Data preparation/2_census_dwelling.rds")

names(censo_viv)

table(censo_viv$v_pers_dormit, useNA = "ifany")
table(censo_viv$v_total_pers, useNA = "ifany")
table(censo_viv$v_total_dorm, useNA = "ifany")
table(censo_viv$v_lugar_proviene_agua, useNA = "ifany")

vivienda = censo_viv %>% 
  mutate(h_numero_cuartos = as.numeric(v_total_dorm),
         h_num_perhog = as.numeric(v_total_pers),
         hacinamiento = case_when((h_numero_cuartos)==0 ~ 0,
                                  (h_num_perhog)/(h_numero_cuartos) > 3 ~ 1,
                                  T ~ 0))%>% 
  mutate(agua_proviene = ifelse(v_lugar_proviene_agua == "1", 1, 0))

table(vivienda$hacinamiento, vivienda$v_pers_dormit, useNA = "ifany")
prop.table(table(vivienda$hacinamiento))
sum(vivienda$hacinamiento)


# Para el área urbana se consideró como material de bienestar muros de hormigón armado o
# albañilería, mientras que para el área rural se consideró como material de bienestar muros de
# hormigón armado, albañilería o tabique forrado (ladrillo) por ambas caras.

vivienda = vivienda %>% 
  mutate(muros_con_bienes = ifelse(area == "1" & v_material_paredes == "1", 1,
                                   ifelse(area == "2" & v_material_paredes %in% c ("1","2"), 1, 0 ))) 

sum(vivienda$muros_con_bienes)
prop.table(table(vivienda$muros_con_bienes))


# materialidad alto

vivienda = vivienda %>% 
  mutate(materialidad = ifelse(v_estado_paredes == "1" & v_estado_piso == "1" & v_estado_techo == "1", 1, 0 ))

sum(vivienda$materialidad)
prop.table(table(vivienda$materialidad))


# Otras variables segun el doc metodologico (INEC, 2021) de Ecuador
table(vivienda$v_lugar_proviene_luz, useNA = "ifany")
table(vivienda$v_via_acceso, useNA = "ifany")
table(vivienda$v_agua_es, useNA = "ifany") # recibe agua
table(vivienda$v_serv_higienico_es, useNA = "ifany")
table(vivienda$v_eliminacion_basura, useNA = "ifany")
table(vivienda$v_dispone_medidor, useNA = "ifany")
table(vivienda$v_lugar_proviene_luz, useNA = "ifany")

vivienda = vivienda %>% 
  mutate(dummy_proviene_luz = ifelse(v_lugar_proviene_luz %in% c("1","2","3"), 1, 0),
         dummy_recibe_agua = ifelse(v_agua_es %in% c("1"), 1, 0),
         dummy_serv_higienico = ifelse(v_serv_higienico_es %in% c("1"), 1, 0),
         dummy_eliminacion_basura = ifelse(v_eliminacion_basura %in% c("1"), 1, 0),
         # dispone de luz electrica
         dispone_luz_num = ifelse(v_lugar_proviene_luz %in% c("1","2","3","4"), 1, 0),
         dispone_luz_den = ifelse(v_lugar_proviene_luz %in% c("1","2","3","4","5"), 1, 0)
         )


# Agregacion a nivel de UPM
upm_viv = vivienda %>% 
  group_by(id_upm) %>% 
  summarise(total_viviendas = n(),
            hacinamiento = sum(hacinamiento, na.rm = T),
            agua_proviene = sum(agua_proviene, na.rm = T),
            muros_con_bienes = sum(muros_con_bienes, na.rm = T),
            materialidad = sum(materialidad, na.rm = T),
            # Otras variables
            dummy_proviene_luz = sum(dummy_proviene_luz, na.rm = T),
            dummy_recibe_agua = sum(dummy_recibe_agua, na.rm = T),
            dummy_serv_higienico = sum(dummy_serv_higienico, na.rm = T),
            dummy_eliminacion_basura = sum(dummy_eliminacion_basura, na.rm = T),
            dispone_luz_num = sum(dispone_luz_num, na.rm = T),
            dispone_luz_den = sum(dispone_luz_den, na.rm = T)
            ) %>% 
  mutate(v_no_hacin = round(100 - hacinamiento/total_viviendas * 100, 2),
         v_agua = round(agua_proviene/total_viviendas * 100, 2),
         v_muros = round(muros_con_bienes/total_viviendas * 100, 2),
         v_materialidad = round(materialidad/total_viviendas * 100, 2),
         # Otras variables
         v_proviene_luz = round(dummy_proviene_luz/total_viviendas * 100, 2),
         v_recibe_agua = round(dummy_recibe_agua/total_viviendas * 100, 2),
         v_serv_higienico = round(dummy_serv_higienico/total_viviendas * 100, 2),
         v_eliminacion_basura = round(dummy_eliminacion_basura/total_viviendas * 100, 2),
         v_dispone_luz = round(dispone_luz_num / dispone_luz_den * 100, 2)
         )


# Analisis descriptivo
summary(upm_viv$v_no_hacin)
summary(upm_viv$v_agua)
summary(upm_viv$v_muros)
summary(upm_viv$v_materialidad)

summary(upm_viv$v_proviene_luz)
summary(upm_viv$v_recibe_agua)
summary(upm_viv$v_serv_higienico)
summary(upm_viv$v_eliminacion_basura)

summary(upm_viv$v_dispone_luz)

# Guardar las bases de datos
saveRDS(vivienda, "./Data preparation/1_Data preparation/5_dwelling.rds")
saveRDS(upm_viv, "./Data preparation/1_Data preparation/5_PSU_dwelling.rds")
