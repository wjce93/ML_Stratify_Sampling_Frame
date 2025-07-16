rm(list = ls())

library(rio)
library(tidyverse)
library(openxlsx)

# Apertura de la base de datos de hogares
censo_hog = readRDS("./Data preparation/1_Data preparation/3_census_household.rds")
names(censo_hog)

table(censo_hog$h_cuarto_cocinar, useNA = "ifany")
table(censo_hog$h_serv_higienico_es, useNA = "ifany")
table(censo_hog$h_dispone_ducha, useNA = "ifany")
table(censo_hog$h_dispone_telef, useNA = "ifany")
table(censo_hog$h_dispone_celular, useNA = "ifany")
table(censo_hog$h_dispone_internet, useNA = "ifany")
table(censo_hog$h_dispone_compu, useNA = "ifany")
table(censo_hog$h_dispone_tvcable, useNA = "ifany")


# Construccion de variables
hogares = censo_hog %>%
  mutate(dummy_serv_higienico_es = ifelse(h_serv_higienico_es %in% c("1"), 1, 0),
         dummy_lugar_cocinar = ifelse(h_cuarto_cocinar %in% c("1"), 1, 0),
         dummy_dispone_ducha = ifelse(h_dispone_ducha %in% c("1"), 1, 0),
         dummy_dispone_telef = ifelse(h_dispone_telef %in% c("1"), 1, 0),
         dummy_dispone_celular = ifelse(h_dispone_celular %in% c("1"), 1, 0),
         dummy_dispone_internet = ifelse(h_dispone_internet %in% c("1"), 1, 0),
         dummy_dispone_compu = ifelse(h_dispone_compu %in% c("1"), 1, 0),
         dummy_dispone_tvcable = ifelse(h_dispone_tvcable %in% c("1"), 1, 0),
         equipamiento = dummy_dispone_telef + dummy_dispone_celular + dummy_dispone_internet +
           dummy_dispone_compu + dummy_dispone_tvcable,
         dummy_todos_bienes = ifelse(equipamiento == 5, 1, 0)
         )

summary(hogares$equipamiento)
table(hogares$dummy_todos_bienes)

# Agregacion a nivel de UMP
upm_hog = hogares %>%
  group_by(id_upm, area) %>%
  summarise(dummy_serv_higienico_es = sum(dummy_serv_higienico_es, na.rm = T),
            dummy_lugar_cocinar = sum(dummy_lugar_cocinar, na.rm = T),
            dummy_dispone_ducha = sum(dummy_dispone_ducha),
            dummy_dispone_telef = sum(dummy_dispone_telef, na.rm = T),
            dummy_dispone_celular = sum(dummy_dispone_celular, na.rm = T),
            dummy_dispone_internet = sum(dummy_dispone_internet, na.rm = T),
            dummy_dispone_compu = sum(dummy_dispone_compu, na.rm = T),
            dummy_dispone_tvcable = sum(dummy_dispone_tvcable, na.rm = T),
            dummy_todos_bienes = sum(dummy_todos_bienes, na.rm = T),
            total_hogares = n()
            ) %>%
  ungroup() %>%
  mutate(h_serv_higienico_es = round(dummy_serv_higienico_es/total_hogares * 100, 2),
         h_lugar_cocinar = round(dummy_lugar_cocinar/total_hogares * 100, 2),
         h_dispone_ducha = round(dummy_dispone_ducha/total_hogares * 100, 2),
         h_dispone_telef = round(dummy_dispone_telef/total_hogares * 100, 2),
         h_dispone_celular = round(dummy_dispone_celular/total_hogares * 100, 2),
         h_dispone_internet = round(dummy_dispone_internet/total_hogares * 100, 2),
         h_dispone_compu = round(dummy_dispone_compu/total_hogares * 100, 2),
         h_dispone_tvcable = round(dummy_dispone_tvcable/total_hogares * 100, 2),
         h_todos_bienes = round(dummy_todos_bienes/total_hogares * 100, 2)
         )

# Analisis descriptivo
summary(upm_hog$h_serv_higienico_es)
summary(upm_hog$h_lugar_cocinar)
summary(upm_hog$h_dispone_ducha)
summary(upm_hog$h_dispone_telef)
summary(upm_hog$h_dispone_celular)
summary(upm_hog$h_dispone_internet)
summary(upm_hog$h_dispone_compu)
summary(upm_hog$h_dispone_tvcable)
summary(upm_hog$h_todos_bienes)

# Guardar las bases de datos
saveRDS(hogares, "./Data preparation/1_Data preparation/6_household.rds")
saveRDS(upm_hog, "./Data preparation/1_Data preparation/6_PSU_household.rds")
