
rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)
library(magrittr)

# Apertura de la base con los estratos a nivel de Cuenca (3, 4 y 5 grupos), estratificacion clasica y
# machine learning
consolidado = readRDS("./Data preparation/4_Coincidence matrix/consolidated_stratification_CLA_ML.rds")

consolidado %<>%
  select(id_sector,
         estratos_cla_3 = LH_Kozak_k3,
         estratos_cla_4 = LH_Kozak_k4,
         estratos_cla_5 = Dalenius_k5,
         estratos_ml_3 = MiniBatchKMeans_k3,
         estratos_ml_4 = KMeans_k4,
         estratos_ml_5 = KMeans_k5
         )

# Proporcion de UPM en los estratos
prop.table(table(consolidado$estratos_cla_3, useNA = "ifany"))
prop.table(table(consolidado$estratos_cla_4, useNA = "ifany"))
prop.table(table(consolidado$estratos_cla_5, useNA = "ifany"))

prop.table(table(consolidado$estratos_ml_3, useNA = "ifany"))
prop.table(table(consolidado$estratos_ml_4, useNA = "ifany"))
prop.table(table(consolidado$estratos_ml_5, useNA = "ifany"))

# Union global de clasicos y machine learning
consolidado = consolidado %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.integer, as.character)

# Guardar los resultados de la mejor estratificacion para Cuenca
saveRDS(consolidado, "./Data preparation/5_Boxplots/best_stratification_CLA_ML.rds")
