rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Apertura de la base
base = readRDS("./Data preparation/1_Data preparation/8_PSU_indicators.rds")

base = base %>%
  mutate(provincia = substr(id_upm, 1, 2),
         canton = substr(id_upm, 1, 4),
         parroquia = substr(id_upm, 1, 6)) %>%
  # filter(provincia == "01") %>%
  filter(parroquia == "010150") %>%
  # select(-canton) %>%
  select(-c(provincia, canton, parroquia))

# Guardar la base
# saveRDS(base, "./Data preparation/1_Data preparation/9_PSU_indicators_Quito.rds")
# saveRDS(base, "./Data preparation/1_Data preparation/10_PSU_indicators_Quito_p.rds")
saveRDS(base, "./Data preparation/1_Data preparation/9_PSU_indicators_Cuenca.rds")
write.xlsx(base, "./Data preparation/1_Data preparation/9_PSU_indicators_Cuenca.xlsx")
