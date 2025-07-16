rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Union de las variables de poblacion, vivienda y hogar
upm_pob = readRDS("./Data preparation/1_Data preparation/4_PSU_population.rds")
upm_viv = readRDS("./Data preparation/1_Data preparation/5_PSU_dwelling.rds")
upm_hog = readRDS("./Data preparation/1_Data preparation/6_PSU_household.rds")

base = upm_pob %>%
  full_join(upm_viv, by = "id_upm") %>%
  full_join(upm_hog, by = "id_upm")

names(base)

# Revision de valores perdidos
apply(is.na(base), 2, sum)
sum(apply(is.na(base), 2, sum))

# Construccion de la variable dominio y area
names(base)
base = base %>%
  mutate(prov = substr(id_upm, 1, 2),
         ciudad = substr(id_upm, 1, 6),
         ciudad_auto = ifelse(ciudad == "170150" & area == "1", "25",
                              ifelse(ciudad == "090150" & area == 1, "26",
                                     ifelse(ciudad == "010150" & area == "1", "27",
                                            ifelse(ciudad == "070150" & area == 1, "28",
                                                   ifelse(ciudad == "180150" & area == "1", "29", NA))))),
         dom = ifelse(!is.na(ciudad_auto), ciudad_auto, prov),
         dom_area = paste0(dom,area)
         )

table(base$ciudad_auto, useNA = "ifany")
table(base$prov, useNA = "ifany")
table(base$dom, useNA = "ifany")
table(base$dom_area, useNA = "ifany")

table(base$dom, base$prov, useNA = "ifany")

n_distinct(base$dom)
n_distinct(base$dom_area)

# Ordenar las variables
base = base %>%
  select(id_upm, prov, ciudad, dom, area, dom_area,
         educ_superior:v_dispone_luz, 
         dummy_serv_higienico_es:h_todos_bienes)

sum(apply(is.na(base), 2, sum))

# Guardar la base de datos
saveRDS(base, "./Data preparation/1_Data preparation/7_PSU_global.rds")
