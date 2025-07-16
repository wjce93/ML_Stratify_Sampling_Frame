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
data_cla_g = melt(data[,c(23,20,29,40,44)], # seleccionar las variables
                  id="estratos_cla_5", 
                  variable.name = "variable", 
                  value.name = "value")

nombres = c("Mean schooling of the census sector", "Affiliation to private pension systems",
            "Water collection method", "Cable TV service")

gra_cla = data_cla_g %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = estratos_cla_5)) + 
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  scale_fill_manual(values = c("darkseagreen2","springgreen4","rosybrown1","lightblue2","orange")) +
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
ggsave(file = "grafico_cl_5.png",
       plot = gra_cla,
       device = "png",
       path = "./Results/4_Boxplots",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)



# Grafico del boxplot (estratificacion machine learning)
data_ml_g = melt(data[,c(23,20,29,40,47)], # seleccionar las variables
                 id="estratos_ml_5", 
                 variable.name = "variable", 
                 value.name = "value")

gra_ml = data_ml_g %>%
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = estratos_ml_5)) + 
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  scale_fill_manual(values = c("darkseagreen2","springgreen4","rosybrown1","lightblue2","orange")) +
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
ggsave(file = "grafico_ml_5.png",
       plot = gra_ml,
       device = "png",
       path = "./Results/4_Boxplots",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)
