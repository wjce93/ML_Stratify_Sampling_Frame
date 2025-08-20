rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)
library(magrittr)
library(ggplot2)
library(reshape2)
library(viridis)

# Apertura de los estratos 
estratos = readRDS("./Data preparation/5_Boxplots/best_stratification_CLA_ML.rds")

# Apertura de la base de indicadores a nivel de UPM
base = readRDS("./Data preparation/1_Data preparation/9_PSU_indicators_Cuenca.rds")
indicadores = base[,c(1,4:dim(base)[2])] %>% rename(id_sector = id_upm)

# Union de los estratos y de la base de indicadores
data = indicadores %>%
  left_join(estratos, by = "id_sector")

apply(is.na(data), 2, sum)
rm(base, estratos)

# Grafico del boxplot (estratificacion clasica)
data_cla_g = melt(data[,c(22,21,27,37,43)], # seleccionar las variables
                  id="estratos_cla_4", 
                  variable.name = "variable", 
                  value.name = "value")

nombres = c("Educ. attainment of the head of household", "Affiliation to social security",
            "Material index", "Cell/mobile phone with service")

gra_cla = data_cla_g %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = estratos_cla_4)) + 
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  scale_fill_manual(values = c("darkseagreen2","rosybrown1","lightblue2","orange")) +
  labs(fill = "Strata") +
  ggtitle("") +
  xlab("Variable") + 
  ylab("Proportion (%)") +
  scale_x_discrete(labels = nombres) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18)) 

print(gra_cla)

# Guardar el grafico
ggsave(file = "grafico_cl_4.png",
       plot = gra_cla,
       device = "png",
       path = "./Results/4_Boxplots",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)



# Grafico del boxplot (estratificacion machine learning)
data_ml_g = melt(data[,c(22,21,27,37,46)], # seleccionar las variables
                 id="estratos_ml_4", 
                 variable.name = "variable", 
                 value.name = "value")

gra_ml = data_ml_g %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = estratos_ml_4)) + 
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  scale_fill_manual(values = c("darkseagreen2","rosybrown1","lightblue2","orange")) +
  labs(fill = "Strata") +
  ggtitle("") +
  xlab("Variable") + 
  ylab("Proportion (%)") +
  scale_x_discrete(labels = nombres) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18)) 

print(gra_ml)

# Guardar el grafico
ggsave(file = "grafico_ml_4.png",
       plot = gra_ml,
       device = "png",
       path = "./Results/4_Boxplots",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)
