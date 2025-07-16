rm(list = ls())

library(tidyverse)
library(rio)

# Seleccion de las mejores estratificaciones segun el metodo de clustering y representacion

# Apertura del conjunto de datos de UPM de Cuenca
upm_cuenca = readRDS("./Data preparation/1_Data preparation/9_PSU_indicators_Cuenca.rds")

# Apertura de las mejores estratificaciones
estratos_best = readRDS("./Results/2_Machine learning stratification/2_Score metrics indexes/summary_metrics_clustering_best.rds")

# Apertura de las etiquetas
etiquetas = read.csv("./Results/2_Machine learning stratification/1_Results metrics clustering/etiquetas_finales_cross_validation.csv")
etiquetas = etiquetas[,-c(1)]
vnombres = as.data.frame(names(etiquetas))

names(etiquetas)

# Seleccion de las etiquetas de las mejores estratificaciones

# KernelPCA (kernel=rbf, gamma=1)
# Agglomerative (k=3, linkage=ward)
# KernelPCA..kernel.rbf..gamma.1....Agglomerative..k.3..linkage.ward.

# Crear vector de nombres de columna en formato del dataframe 'etiquetas'
nombres_columnas = paste(estratos_best$Representación, estratos_best$Método, sep = "   ")

# Limpiar caracteres especiales para que coincidan con los nombres en 'etiquetas'
nombres_columnas_limpios = gsub("[^a-zA-Z0-9]", ".", nombres_columnas)

# Seleccionar columnas válidas que existen en el dataframe
columnas_validas = intersect(nombres_columnas_limpios, colnames(etiquetas))

# Extraerlas
etiquetas_filtradas = etiquetas[, columnas_validas, drop = FALSE]

# Cambiar de nombres a las variables del dataframe de mejores estratificaciones
names(etiquetas_filtradas)


# "KernelPCA..kernel.rbf..gamma.1....Agglomerative..k.3..linkage.ward." en "Agglomerative_k_3" 

# Obtener nombres originales
nombres_originales = colnames(etiquetas_filtradas)

# Función general para extraer el método y el número de clusters (k)
renombrar_metodo_k = function(nombre) {
  # Extrae patrón tipo "Agglomerative..k.3" o "BIRCH..k.3" o similar
  match <- regmatches(nombre, regexpr("[A-Za-z]+\\.\\.k\\.[0-9]+", nombre))
  # Limpia el nombre: reemplaza ".." por "_" y quita puntos extra
  limpio <- gsub("\\.\\.", "_", match)
  limpio <- gsub("\\.", "", limpio)
  return(limpio)
}

# Aplica a todos los nombres
colnames(etiquetas_filtradas) = sapply(nombres_originales, renombrar_metodo_k)

names(etiquetas_filtradas)


# Resumen de las proporciones entre los estratos de los mejores metodos de agrupamiento
# Calcular proporciones para todas las columnas del dataframe
# Crear una lista con proporciones y conteos absolutos
proporciones <- lapply(etiquetas_filtradas, function(col) {
  tbl <- table(col)
  prop <- prop.table(tbl)
  data.frame(Valor = names(tbl), Frecuencia = as.vector(tbl), Proporcion = as.vector(prop))
})

proporciones_df <- do.call(rbind, Map(function(df, nombre) {
  df$Variable <- nombre
  df
}, proporciones, names(proporciones)))

# Renombrar columnas
colnames(proporciones_df) <- c("Valor", "Frecuencia", "Proporcion", "Variable")

# Reorganizar columnas
proporciones_df <- proporciones_df[, c("Variable", "Valor", "Frecuencia", "Proporcion")]


# Ordenamiento de los estratos en funcion a un indicador
# Apertura de la base de indicadores para determinar el orden de los clusters
## Viviendas
personas_deff = readRDS("./Data preparation/1_Data preparation/4_population_deff.rds") %>%
  filter(ciudad == "010150")
viviendas_deff = readRDS("./Data preparation/1_Data preparation/5_dwelling_deff.rds") %>%
  filter(id_sector %in% unique(personas_deff$id_sector))

rm(personas_deff)

# Base de estratos
estratos = cbind(select(upm_cuenca, id_upm), etiquetas_filtradas)

# Union con los estratos generados
viviendas_deff_est = viviendas_deff %>%
  left_join(estratos, by = c("id_sector"="id_upm"))

apply(is.na(viviendas_deff_est), 2, sum)

table(viviendas_deff_est$v_materialidad, useNA = "ifany")

# Apertura de la funcion para ordenar los estratos
# ejemplo
# viviendas_deff_est %>%
#   group_by(Spectral_k3) %>%
#   summarise(p = mean(v_materialidad))


# Recodificacion de los estratos en funcion al indice de materialidad
# Extraer nombres de columnas de estratos (ej. todas las que empiecen por "cluster_")
cols_estratos <- grep("k", names(viviendas_deff_est), value = TRUE)

# Recodificar cada columna
for (col in cols_estratos) {
  # Calcular proporción de v_materialidad == 1 por grupo
  proporciones_v <- viviendas_deff_est %>%
    group_by(cluster = .data[[col]]) %>%
    summarise(prop = mean(v_materialidad, na.rm = TRUE), .groups = 'drop') %>%
    arrange(prop) %>%
    mutate(new_label = row_number())
  
  # Crear vector de mapeo
  recodificacion <- setNames(proporciones_v$new_label, proporciones_v$cluster)
  
  # Reemplazar valores con nuevos labels
  viviendas_deff_est[[col]] <- recodificacion[as.character(viviendas_deff_est[[col]])]
}

# Ver sectores con múltiples estratos (conflictos)
conflictos <- viviendas_deff_est %>%
  group_by(id_sector) %>%
  summarise(across(all_of(cols_estratos), ~ n_distinct(.))) %>%
  filter(if_any(everything(), ~ . > 1))

# Conjunto de datos agrupado a nivel de UPM
estratos_upm <- viviendas_deff_est %>%
  group_by(id_sector) %>%
  summarise(across(all_of(cols_estratos), ~ {
    vals <- unique(.)
    if (length(vals) == 1) {
      vals
    } else {
      NA  # o usar NA para marcar conflicto, o paste(vals, collapse = ",")
    }
  }), .groups = "drop")

# Guardar los estratos obtenidos para el calculo del deff
saveRDS(estratos_upm, "./Data preparation/3_Machine learning stratification/strata_ML_PSU_Cuenca.rds")
