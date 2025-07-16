rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Funciones necesarias
source("./Scripts/2_Classic stratification/0_functions.R")

# Apertura de los resultados de la estratificacion
estratificacion_3 = readRDS("./Data preparation/3_Machine learning stratification/strata_ML_PSU_Cuenca.rds") %>%
  select(id_sector, grep("3", names(.), value = T))
estratificacion_4 = readRDS("./Data preparation/3_Machine learning stratification/strata_ML_PSU_Cuenca.rds") %>%
  select(id_sector, grep("4", names(.), value = T))
estratificacion_5 = readRDS("./Data preparation/3_Machine learning stratification/strata_ML_PSU_Cuenca.rds") %>%
  select(id_sector, grep("5", names(.), value = T))

# Apertura de todos los nombres de los indicadores
nombres_indicadores = readRDS("./Data preparation/2_Classic stratification/nombres_indicadores.rds")

# Calculo de los def por hogar, vivienda y personas, con todas las variables de la matriz de indicadores

# Apertura de los conjuntos de datos del censo
personas_deff = readRDS("./Data preparation/1_Data preparation/4_population_deff.rds") %>%
  filter(ciudad == "010150")
viviendas_deff = readRDS("./Data preparation/1_Data preparation/5_dwelling_deff.rds") %>%
  filter(id_sector %in% unique(personas_deff$id_sector))
hogares_deff = readRDS("./Data preparation/1_Data preparation/6_household_deff.rds") %>%
  filter(id_sector %in% unique(personas_deff$id_sector))

# Hogares
# Con 3 estratos
aux_hg_3 <- data.frame(Agglomerative = deff_datas(hogares_deff, "Agglomerative_k3", estratificacion_3, nombres_indicadores),
                       BIRCH = deff_datas(hogares_deff, "BIRCH_k3", estratificacion_3, nombres_indicadores),
                       GMM = deff_datas(hogares_deff, "GMM_k3", estratificacion_3, nombres_indicadores),
                       KMeans = deff_datas(hogares_deff, "KMeans_k3", estratificacion_3, nombres_indicadores),
                       MiniBatchKMeans = deff_datas(hogares_deff, "MiniBatchKMeans_k3", estratificacion_3, nombres_indicadores),
                       Spectral = deff_datas(hogares_deff, "Spectral_k3", estratificacion_3, nombres_indicadores))

# Con 4 estratos
aux_hg_4 <- data.frame(Agglomerative = deff_datas(hogares_deff, "Agglomerative_k4", estratificacion_4, nombres_indicadores),
                       BIRCH = deff_datas(hogares_deff, "BIRCH_k4", estratificacion_4, nombres_indicadores),
                       GMM = deff_datas(hogares_deff, "GMM_k4", estratificacion_4, nombres_indicadores),
                       KMeans = deff_datas(hogares_deff, "KMeans_k4", estratificacion_4, nombres_indicadores),
                       MiniBatchKMeans = deff_datas(hogares_deff, "MiniBatchKMeans_k4", estratificacion_4, nombres_indicadores),
                       Spectral = deff_datas(hogares_deff, "Spectral_k4", estratificacion_4, nombres_indicadores))

# Con 5 estratos
aux_hg_5 <- data.frame(Agglomerative = deff_datas(hogares_deff, "Agglomerative_k5", estratificacion_5, nombres_indicadores),
                       BIRCH = deff_datas(hogares_deff, "BIRCH_k5", estratificacion_5, nombres_indicadores),
                       GMM = deff_datas(hogares_deff, "GMM_k5", estratificacion_5, nombres_indicadores),
                       KMeans = deff_datas(hogares_deff, "KMeans_k5", estratificacion_5, nombres_indicadores),
                       MiniBatchKMeans = deff_datas(hogares_deff, "MiniBatchKMeans_k5", estratificacion_5, nombres_indicadores),
                       Spectral = deff_datas(hogares_deff, "Spectral_k5", estratificacion_5, nombres_indicadores))

rm(hogares_deff)

# Viviendas
# Con 3 estratos
aux_viv_3 <- data.frame(Agglomerative = deff_datas(viviendas_deff, "Agglomerative_k3", estratificacion_3, nombres_indicadores),
                        BIRCH = deff_datas(viviendas_deff, "BIRCH_k3", estratificacion_3, nombres_indicadores),
                        GMM = deff_datas(viviendas_deff, "GMM_k3", estratificacion_3, nombres_indicadores),
                        KMeans = deff_datas(viviendas_deff, "KMeans_k3", estratificacion_3, nombres_indicadores),
                        MiniBatchKMeans = deff_datas(viviendas_deff, "MiniBatchKMeans_k3", estratificacion_3, nombres_indicadores),
                        Spectral = deff_datas(viviendas_deff, "Spectral_k3", estratificacion_3, nombres_indicadores))

# Con 4 estratos
aux_viv_4 <- data.frame(Agglomerative = deff_datas(viviendas_deff, "Agglomerative_k4", estratificacion_4, nombres_indicadores),
                        BIRCH = deff_datas(viviendas_deff, "BIRCH_k4", estratificacion_4, nombres_indicadores),
                        GMM = deff_datas(viviendas_deff, "GMM_k4", estratificacion_4, nombres_indicadores),
                        KMeans = deff_datas(viviendas_deff, "KMeans_k4", estratificacion_4, nombres_indicadores),
                        MiniBatchKMeans = deff_datas(viviendas_deff, "MiniBatchKMeans_k4", estratificacion_4, nombres_indicadores),
                        Spectral = deff_datas(viviendas_deff, "Spectral_k4", estratificacion_4, nombres_indicadores))

# Con 5 estratos
aux_viv_5 <- data.frame(Agglomerative = deff_datas(viviendas_deff, "Agglomerative_k5", estratificacion_5, nombres_indicadores),
                        BIRCH = deff_datas(viviendas_deff, "BIRCH_k5", estratificacion_5, nombres_indicadores),
                        GMM = deff_datas(viviendas_deff, "GMM_k5", estratificacion_5, nombres_indicadores),
                        KMeans = deff_datas(viviendas_deff, "KMeans_k5", estratificacion_5, nombres_indicadores),
                        MiniBatchKMeans = deff_datas(viviendas_deff, "MiniBatchKMeans_k5", estratificacion_5, nombres_indicadores),
                        Spectral = deff_datas(viviendas_deff, "Spectral_k5", estratificacion_5, nombres_indicadores))

rm(viviendas_deff)

## Personas
# Con 3 estratos
aux_per_3 <- data.frame(Agglomerative = deff_datas(personas_deff, "Agglomerative_k3", estratificacion_3, nombres_indicadores),
                        BIRCH = deff_datas(personas_deff, "BIRCH_k3", estratificacion_3, nombres_indicadores),
                        GMM = deff_datas(personas_deff, "GMM_k3", estratificacion_3, nombres_indicadores),
                        KMeans = deff_datas(personas_deff, "KMeans_k3", estratificacion_3, nombres_indicadores),
                        MiniBatchKMeans = deff_datas(personas_deff, "MiniBatchKMeans_k3", estratificacion_3, nombres_indicadores),
                        Spectral = deff_datas(personas_deff, "Spectral_k3", estratificacion_3, nombres_indicadores))

# Con 4 estratos
aux_per_4 <- data.frame(Agglomerative = deff_datas(personas_deff, "Agglomerative_k4", estratificacion_4, nombres_indicadores),
                        BIRCH = deff_datas(personas_deff, "BIRCH_k4", estratificacion_4, nombres_indicadores),
                        GMM = deff_datas(personas_deff, "GMM_k4", estratificacion_4, nombres_indicadores),
                        KMeans = deff_datas(personas_deff, "KMeans_k4", estratificacion_4, nombres_indicadores),
                        MiniBatchKMeans = deff_datas(personas_deff, "MiniBatchKMeans_k4", estratificacion_4, nombres_indicadores),
                        Spectral = deff_datas(personas_deff, "Spectral_k4", estratificacion_4, nombres_indicadores))

# Con 5 estratos
aux_per_5 <- data.frame(Agglomerative = deff_datas(personas_deff, "Agglomerative_k5", estratificacion_5, nombres_indicadores),
                        BIRCH = deff_datas(personas_deff, "BIRCH_k5", estratificacion_5, nombres_indicadores),
                        GMM = deff_datas(personas_deff, "GMM_k5", estratificacion_5, nombres_indicadores),
                        KMeans = deff_datas(personas_deff, "KMeans_k5", estratificacion_5, nombres_indicadores),
                        MiniBatchKMeans = deff_datas(personas_deff, "MiniBatchKMeans_k5", estratificacion_5, nombres_indicadores),
                        Spectral = deff_datas(personas_deff, "Spectral_k5", estratificacion_5, nombres_indicadores))

rm(personas_deff)

# Resumen
# 3 estratos
resumen_H3 <- rbind(aux_viv_3, aux_hg_3, aux_per_3) 
resumen_H3 <- rbind(resumen_H3, colSums(resumen_H3)) 
row.names(resumen_H3)[nrow(resumen_H3)] <-  "G(S)"

rm(aux_hg_3, aux_viv_3, aux_per_3)

deff_sum_3 <- (resumen_H3 |> filter(row_number()>n()-1)) 
min_deff_3 <- min(deff_sum_3)
index_met_3 <- which(deff_sum_3 == min_deff_3)

# 4 estratos
resumen_H4 <- rbind(aux_viv_4, aux_hg_4, aux_per_4) 
resumen_H4 <- rbind(resumen_H4, colSums(resumen_H4)) 
row.names(resumen_H4)[nrow(resumen_H4)] <-  "G(S)"

rm(aux_hg_4, aux_viv_4, aux_per_4)

deff_sum_4 <- (resumen_H4 |> filter(row_number()>n()-1)) 
min_deff_4 <- min(deff_sum_4)
index_met_4 <- which(deff_sum_4 == min_deff_4)

# 5 estratos
resumen_H5 <- rbind(aux_viv_5, aux_hg_5, aux_per_5) 
resumen_H5 <- rbind(resumen_H5, colSums(resumen_H5)) 
row.names(resumen_H5)[nrow(resumen_H5)] <-  "G(S)"

rm(aux_hg_5, aux_viv_5, aux_per_5)

deff_sum_5 <- (resumen_H5 |> filter(row_number()>n()-1)) 
min_deff_5 <- min(deff_sum_5)
index_met_5 <- which(deff_sum_5 == min_deff_5)

# Guardar las datos de los resumenes
saveRDS(resumen_H3, "./Results/2_Machine learning stratification/3_Deff Cuenca/deff_3.rds")
saveRDS(resumen_H4, "./Results/2_Machine learning stratification/3_Deff Cuenca/deff_4.rds")
saveRDS(resumen_H5, "./Results/2_Machine learning stratification/3_Deff Cuenca/deff_5.rds")

write.xlsx(resumen_H3, "./Results/2_Machine learning stratification/3_Deff Cuenca/deff_3.xlsx")
write.xlsx(resumen_H4, "./Results/2_Machine learning stratification/3_Deff Cuenca/deff_4.xlsx")
write.xlsx(resumen_H5, "./Results/2_Machine learning stratification/3_Deff Cuenca/deff_5.xlsx")
