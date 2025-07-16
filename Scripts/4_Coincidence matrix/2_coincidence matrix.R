rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)
library(magrittr)

# Crear una función que calcula la similitud entre dos columnas
similarity <- function(x, y) {
  mean(x == y, na.rm = TRUE) * 100  # porcentaje de coincidencia
}

# Apertura de los resultados de las estratificaciones para Cuenca
estratos = readRDS("./Data preparation/4_Coincidence matrix/consolidated_stratification_CLA_ML.rds")

estratos = estratos %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.integer, as.character)

# Seleccion de la estratificacion de k=3. k=4 y k=5
selected_k3 <- grep("k3", names(estratos), value = T)
df_k3 <- estratos[, selected_k3]
selected_k4 <- grep("k4", names(estratos), value = T)
df_k4 <- estratos[, selected_k4]
selected_k5 <- grep("k5", names(estratos), value = T)
df_k5 <- estratos[, selected_k5]

# Obtener nombres de las variables
vars_k3 <- names(df_k3)
vars_k4 <- names(df_k4)
vars_k5 <- names(df_k5)

# Crear una matriz vacía
n <- length(vars_k3)
similarity_matrix_k3 <- matrix(0, nrow = n, ncol = n)
rownames(similarity_matrix_k3) <- vars_k3
colnames(similarity_matrix_k3) <- vars_k3

similarity_matrix_k4 <- matrix(0, nrow = n, ncol = n)
rownames(similarity_matrix_k4) <- vars_k4
colnames(similarity_matrix_k4) <- vars_k4

similarity_matrix_k5 <- matrix(0, nrow = n, ncol = n)
rownames(similarity_matrix_k5) <- vars_k5
colnames(similarity_matrix_k5) <- vars_k5

# Llenar la matriz con los porcentajes de similitud
for (i in 1:n) {
  for (j in 1:n) {
    similarity_matrix_k3[i, j] <- similarity(df_k3[[vars_k3[i]]], df_k3[[vars_k3[j]]])
  }
}

for (i in 1:n) {
  for (j in 1:n) {
    similarity_matrix_k4[i, j] <- similarity(df_k4[[vars_k4[i]]], df_k4[[vars_k4[j]]])
  }
}

for (i in 1:n) {
  for (j in 1:n) {
    similarity_matrix_k5[i, j] <- similarity(df_k5[[vars_k5[i]]], df_k5[[vars_k5[j]]])
  }
}

# Convertir a data.frame si lo deseas
similarity_df_k3 <- as.data.frame(similarity_matrix_k3)
similarity_df_k4 <- as.data.frame(similarity_matrix_k4)
similarity_df_k5 <- as.data.frame(similarity_matrix_k5)

rm(similarity_matrix_k3, similarity_matrix_k4, similarity_matrix_k5) 

# Unir los tres resultados
names(similarity_df_k3) <- gsub("_k3", "", names(similarity_df_k3))
names(similarity_df_k4) <- gsub("_k4", "", names(similarity_df_k4))
names(similarity_df_k5) <- gsub("_k5", "", names(similarity_df_k5))

identical(names(similarity_df_k3), names(similarity_df_k4))
identical(names(similarity_df_k4), names(similarity_df_k5))

similarity_global = rbind(similarity_df_k3, similarity_df_k4, similarity_df_k5)

# Guardar los resultados
saveRDS(similarity_global, "./Results/3_Coincidence matrix/coincidence matrix.rds")
write.xlsx(similarity_global, "./Results/3_Coincidence matrix/coincidence matrix.xlsx", rowNames = T)
