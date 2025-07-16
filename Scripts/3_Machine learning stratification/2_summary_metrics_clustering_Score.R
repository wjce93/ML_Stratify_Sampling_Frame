rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)
library(stringr)
library(magrittr)
library(PMCMRplus)
library(scmamp)

# Resultados de las métricas de calidad de la estratificación por representación y método de clustering basado en machine learning
score_df = read.csv("./Results/2_Machine learning stratification/1_Results metrics clustering/score_normalizado_por_metodo.csv")

unique(score_df$Método)

# Agrupar por Método y seleccionar las filas con el Score_Norm_Por_Metodo más alto dentro de cada grupo
score_metodo_df = score_df %>%
  mutate(
    Metodo_Base = str_extract(Método, "^[^(]+") %>% str_trim(),  # Extrae el nombre del método
    Num_Grupos = str_extract(Método, "k=\\d+") %>%                # Extrae "k=5"
      str_remove("k=") %>%                            # Elimina "k="
      as.integer()
  ) %>%
  group_by(Metodo_Base, Num_Grupos) %>%
  arrange(desc(Score_Norm_Por_Metodo)) %>%
  slice_head(n = 1) %>%
  ungroup()

table(score_metodo_df$Metodo_Base, score_metodo_df$Num_Grupos, useNA = "ifany")

# Prueba de Friedman 
# 1. Agrupar y obtener media por combinación de Representación y Método
df_rendimiento = score_df %>%
  group_by(Representación, Método) %>%
  summarise(Media_Score = mean(Score_Norm_Por_Metodo, na.rm = TRUE), .groups = "drop")

# Filtra solo representaciones que tienen resultados para todos los métodos
completas = df_rendimiento %>%
  group_by(Representación) %>%
  summarise(n = n()) %>%
  filter(n == length(unique(df_rendimiento$Método)))

df_filtrado = df_rendimiento %>%
  filter(Representación %in% completas$Representación)

# 2. Crear matriz: filas = Representaciones, columnas = Métodos
matriz_score = df_filtrado %>%
  pivot_wider(names_from = Método, values_from = Media_Score) %>%
  column_to_rownames("Representación")

# 3. Aplicar prueba de Friedman
friedman_result = friedman.test(as.matrix(matriz_score))

# 4. Ver resultados
print(friedman_result)

# 5. Pruebas Post-hoc test Nemenyi para comparaciones múltiples
nemenyi_result = frdAllPairsNemenyiTest(as.matrix(matriz_score))
print(nemenyi_result)

# Extrae las estadísticas de la prueba de Nemenyi
nemenyi_df <- as.data.frame(nemenyi_result$p.value)

# Opcional: si deseas que las combinaciones estén como columnas separadas
nemenyi_df <- tibble::rownames_to_column(nemenyi_df, var = "Comparación")

# Guardar los resultados
saveRDS(score_metodo_df, "./Results/2_Machine learning stratification/2_Score metrics indexes/summary_metrics_clustering_best.rds")
write.xlsx(score_metodo_df, "./Results/2_Machine learning stratification/2_Score metrics indexes/summary_metrics_clustering_best.xlsx")

saveRDS(nemenyi_df, "./Results/2_Machine learning stratification/2_Score metrics indexes/nemenyi_pvalue.rds")
write.xlsx(nemenyi_df, "./Results/2_Machine learning stratification/2_Score metrics indexes/nemenyi_pvalue.xlsx")
