rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Apertura de la base global
base = readRDS("./Data preparation/1_Data preparation/7_PSU_global.rds")

# Seleccion de indicadores estandarizados
base = select(base, id_upm, area, dom_area,
              starts_with("p_"), starts_with("v_"),
              starts_with("h_"))

base = base %>%
  select(-c(p_educ_media, p_educ_baja, p_pers_geningreso))

# Guardar la base
saveRDS(base, "./Data preparation/1_Data preparation/8_PSU_indicators.rds")
