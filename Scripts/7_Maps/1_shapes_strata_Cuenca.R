
rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)
library(magrittr)
library(sf)

# Apertura de los shapes de Cuenca

# Sectores amanzanados
sect_ama = sf::st_read(dsn = "./Data/Shapes Cuenca/Cuenca_amanzanado.shp")

class(sect_ama)

# Sectores dispersos
sect_dis = sf::st_read(dsn = "./Data/Shapes Cuenca/Cuenca_disperso.shp")

# Apertura de los estratos 
estratos = readRDS("./Data preparation/5_Boxplots/best_stratification_CLA_ML.rds")

# Union con los shapes
sect_ama_est = sect_ama %>%
  left_join(estratos, by = c("DPA_SEC" = "id_sector"))

apply(is.na(sect_ama_est), 2, sum)  

sect_dis_est = sect_dis %>%
  left_join(estratos, by = c("DPA_SEC" = "id_sector"))

apply(is.na(sect_dis_est), 2, sum)  

names(sect_ama_est)
names(sect_dis_est)

# Encontrar las columnas comunes
columnas_comunes <- intersect(names(sect_ama_est), names(sect_dis_est))

# Seleccionar solo las columnas comunes en ambos shapes
cuenca_a <- sect_ama_est[, columnas_comunes, drop = F]
cuenca_d <- sect_dis_est[, columnas_comunes, drop = F]

cuenca = rbind(cuenca_a, cuenca_d)
apply(is.na(cuenca), 2, sum)

# Guardar los shapes de Cuenca con la variable de estratificacion
st_write(cuenca, "./Data preparation/6_Maps/Shape_Cuenca.shp")
