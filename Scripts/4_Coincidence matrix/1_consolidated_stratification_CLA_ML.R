rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)
library(magrittr)

# Apertura de la base con los estratos a nivel de Cuenca (3, 4 y 5 grupos), estratificacion clasica y
# machine learning

# Estratificacion clasica

# 3 estratos
est_cla_3 = readRDS("./Data preparation/2_Classic stratification/4_Cuenca/stratification_3.rds")
names(est_cla_3)[2:dim(est_cla_3)[2]] = paste0(names(est_cla_3)[2:dim(est_cla_3)[2]], "_k3")

# 4 estratos
est_cla_4 = readRDS("./Data preparation/2_Classic stratification/4_Cuenca/stratification_4.rds")
names(est_cla_4)[2:dim(est_cla_4)[2]] = paste0(names(est_cla_4)[2:dim(est_cla_4)[2]], "_k4")

# 5 estratos
est_cla_5 = readRDS("./Data preparation/2_Classic stratification/4_Cuenca/stratification_5.rds")
names(est_cla_5)[2:dim(est_cla_5)[2]] = paste0(names(est_cla_5)[2:dim(est_cla_5)[2]], "_k5")

est_cla = est_cla_3 %>%
  left_join(est_cla_4, by = "id_sector") %>%
  left_join(est_cla_5, by = "id_sector")

rm(est_cla_3, est_cla_4, est_cla_5)

# Estratificacion machine learning

# 3, 4 y 5 estratos
est_ml = readRDS("./Data preparation/3_Machine learning stratification/strata_ML_PSU_Cuenca.rds")

# Union 
estratos = est_cla %>% left_join(est_ml, by = "id_sector")

# Guardar los resultados de las estratificaciones para Cuenca
saveRDS(estratos, "./Data preparation/4_Coincidence matrix/consolidated_stratification_CLA_ML.rds")
