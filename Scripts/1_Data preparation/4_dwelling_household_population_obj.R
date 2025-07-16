rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Seleccion de la poblacion objetivo del marco de muestreo 
# viviendas ocupadas con personas presentes y particulares

# Apertura de la base de datos de viviendas
censo_viv = readRDS("./Data/CPV2010_Spss_Vivienda.rds")
str(censo_viv)
class(censo_viv$v_tipo_vivienda)
class(censo_viv$v_condicion_ocu)

censo_viv = censo_viv %>%
  mutate(v_condicion_ocu = str_trim(v_condicion_ocu, side = "both"),
         v_tipo_vivienda = str_trim(v_tipo_vivienda, side = "both")) %>%
  filter(v_condicion_ocu == 1 & v_tipo_vivienda %in% c(1:8)) 

table(censo_viv$v_tipo_vivienda, censo_viv$v_condicion_ocu, useNA = "ifany")

sum(censo_viv$v_total_pers)

# Eliminacion de espacios en blanco
censo_viv = as.data.frame(apply(censo_viv, 2, function(x){
  x = str_trim(x, side = "both")
}))

# Creacion de los identificadores
censo_viv = censo_viv %>%
  mutate(ciudad = paste0(prov,cant,parroq),
         id_upm = paste0(ciudad,zona,sector),
         id_viv = paste0(id_upm,vivienda))

n_distinct(censo_viv$id_viv)
n_distinct(censo_viv$id_upm)
sum(as.numeric(censo_viv$v_total_pers))


# Apertura de la base de hogares
censo_hog = readRDS("./Data/CPV2010_Spss_Hogar.rds")

# Eliminacion de espacios en blanco
censo_hog = as.data.frame(apply(censo_hog, 2, function(x){
  x = str_trim(x, side = "both")
}))

# Creacion de los identificadores
censo_hog = censo_hog %>%
  mutate(ciudad = paste0(prov,cant,parroq),
         id_upm = paste0(ciudad,zona,sector),
         id_viv = paste0(id_upm,vivienda),
         id_hog = paste0(id_viv, hogar))

# Seleccion de los hogares unicamente de la poblacion objeivo
censo_hog = censo_hog %>%
  filter(id_viv %in% censo_viv$id_viv)

# Analisis base de datos
n_distinct(censo_hog$id_viv)
n_distinct(censo_hog$id_hog)
n_distinct(censo_hog$id_upm)
sum(as.numeric(censo_hog$h_tot_per))


# Apertura de la base de personas
censo_per = readRDS("./Data/CPV2010_Spss_Poblacion.rds")

# Eliminacion de espacios en blanco
censo_per = as.data.frame(apply(censo_per, 2, function(x){
  x = str_trim(x, side = "both")
}))

# Creacion de los identificadores
censo_per = censo_per %>%
  mutate(ciudad = paste0(prov,cant,parroq),
         id_upm = paste0(ciudad,zona,sector),
         id_viv = paste0(id_upm,vivienda),
         id_hog = paste0(id_viv, hogar),
         id_per = paste0(id_hog, persona))

# Seleccion de las personas unicamente de la poblacion objetivo
censo_per = censo_per %>%
  # viviendas particulares y ocupadas
  filter(id_viv %in% censo_viv$id_viv) %>%
  # personas que son residentes habituales
  filter(lugar_hab == "1")

# Analisis base de datos
n_distinct(censo_per$id_viv)
n_distinct(censo_per$id_hog)
n_distinct(censo_per$id_upm)
n_distinct(censo_per$id_per)


# Preparacion de los marcos de muestreo finales
# Dado que se selecciono las personas que son residentes habituales, se deben
# seleccionar los hogares y las viviendas de estas personas
viviendas = censo_per %>% group_by(id_viv) %>% summarise()
hogares = censo_per %>% group_by(id_hog) %>% summarise()

censo_viv = censo_viv %>% filter(id_viv %in% viviendas$id_viv)
dim(censo_viv)[1] == dim(viviendas)[1]

censo_hog = censo_hog %>% filter(id_hog %in% hogares$id_hog)
dim(censo_hog)[1] == dim(hogares)[1]


# Guardar las bases de datos de poblacion, vivienda y hogar
saveRDS(censo_per, "./Data preparation/1_Data preparation/1_census_population.rds")
saveRDS(censo_viv, "./Data preparation/1_Data preparation/2_census_dwelling.rds")
saveRDS(censo_hog, "./Data preparation/1_Data preparation/3_census_household.rds")
