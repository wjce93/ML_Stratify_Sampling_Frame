rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)
library(ClustOfVar)
library(stratification)
library(SamplingStrata)
library(factoextra)
library(corrplot)
library(psych)
library(FactoMineR)
library(kableExtra)

# Apertura de la base de indicadores a nivel de UPM
base = readRDS("./Data preparation/1_Data preparation/8_PSU_indicators.rds") %>%
  filter(area == 2)

# Seleccion de variables
indicadores = as.matrix(base[,c(4:dim(base)[2])])

nombres_indicadores = names(as.data.frame(indicadores))
saveRDS(nombres_indicadores, "./Data preparation/2_Classic stratification/nombres_indicadores.rds")

# Análisis de Componentes Principales (ACP)
# Para identificar el número de componentes óptimas se presenta
# el gráfico de sedimentación
scree(indicadores, main ="Grafico de Sedimentacion", factors = F, hline = T)

acp = PCA(indicadores, scale.unit = T, graph = F, ncp = 2)
var_cp1 = acp$eig[1,2]
var_cp1

# Distribución del primer componente principal o medida de resumen
y = acp$ind$coord[,1]

mean_var <- function(x, m2, s2){
  
  m1 <- mean(x)
  s1 <- sd(x)
  if(s1>0){
    y <- m2 + (x-m1)*(s2/s1)}else{
      print("sd es positiva")
    }
  
}
y_trans = mean_var(y, m2 = 50, s2 = 10)
summary(y_trans)

# Se analiza la contribución de las variables  en la contabilidad 
# de la variabilidad de la primera componente
fviz_contrib(acp, choice = "var", axes = 1)
contrib = round(100/ncol(indicadores),2)
contrib

names_contrib_bajo = get_pca_var(acp)$contrib |> as.data.frame() |> select(Dim.1) |> filter(Dim.1 < contrib) |> rownames()

aux_data = indicadores |> as.data.frame() |> select(!all_of(names_contrib_bajo))
acp_cp1 = PCA(aux_data, scale.unit = T, graph = F, ncp = 2)
var_cp1= acp_cp1$eig[1,2]
var_cp1

# Identificacion de las variables que se usan para la estratificación
variables_estratificacion = colnames(aux_data)
saveRDS(variables_estratificacion, "./Data preparation/2_Classic stratification/3_Rural/stratification_variables.rds")

y_cp1 = acp_cp1$ind$coord[,1]
y_trans_cp1 = mean_var(y_cp1, m2 = 50, s2 = 10)
summary(y_trans_cp1)

# Graficos
fviz_pca_var(acp_cp1,
             labelsize = 2,
             col.var = "cos2", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Matriz de indicadores
indicadores = base[,c(1,4:dim(base)[2])] %>%
  rename(id_sector = id_upm)

source("./Scripts/2_Classic stratification/0_functions.R")

# Estratificacion segun 3, 4 y 5 estratos socioeconomicos 

# 3 estratos
univariado_3 <- metodos_univ(y_trans_cp1, H = 3)

resultado_3 = univariado_3 %>%
  mutate(n_estratos = 3) %>%
  select(id_upm = id_sector, n_estratos, percentil, Dalenius, 
         LH = LH_Kozak, geometric)

# 4 estratos
univariado_4 <- metodos_univ(y_trans_cp1, H = 4)

resultado_4 = univariado_4 %>%
  mutate(n_estratos = 4) %>%
  select(id_upm = id_sector, n_estratos, percentil, Dalenius, 
         LH = LH_Kozak, geometric)

# 5 estratos
univariado_5 <- metodos_univ(y_trans_cp1, H = 5)

resultado_5 = univariado_5 %>%
  mutate(n_estratos = 5) %>%
  select(id_upm = id_sector, n_estratos, percentil, Dalenius, 
         LH = LH_Kozak, geometric)

# Union de todos los resultados de la estratificacion
resultado_global = resultado_3 %>%
  rbind(resultado_4) %>%
  rbind(resultado_5)

table(resultado_global$n_estratos, useNA = "ifany")

# Guardar la base de datos con los indicadores
data = base %>%
  select(!all_of(names_contrib_bajo)) %>% select(-dom_area)
export(data, "./Data preparation/2_Classic stratification/3_Rural/rural_stratification_variables.csv")

# Guardar los resultados obtenidos de la estratificacion
saveRDS(resultado_global, "./Results/1_Classic stratification/3_Rural/rural_stratification_classic.rds")

# Guardar las estratificaciones
saveRDS(univariado_3, "./Data preparation/2_Classic stratification/3_Rural/stratification_3.rds")
saveRDS(univariado_4, "./Data preparation/2_Classic stratification/3_Rural/stratification_4.rds")
saveRDS(univariado_5, "./Data preparation/2_Classic stratification/3_Rural/stratification_5.rds")
