rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)
library(magrittr)
library(ggplot2)
library(stringr)

# Graficos de los tiempos empleados en la estratificacion por metodos de machine learning

# Apertura del conjunto de datos
datos = read.csv("./Results/2_Machine learning stratification/1_Results metrics clustering/score_normalizado_por_metodo.csv")

# Creacion de las variables del metodo de clustering y la tecnica de representacion
datos = datos %>%
  rename(representacion = Representación, metodo = Método) %>%
  mutate(metodo_u = str_extract(metodo, "^\\w+"),
         representacion_u = str_extract(representacion, "^\\w+"),
         estratos = as.character(str_extract(metodo, "(?<=k=)\\d+")))

# Comparacion del tiempo promedio por metodo
t_metodo = datos %>%
  mutate(metodo_est = paste0(metodo_u, " (k=", estratos, ")")) %>%
  group_by(metodo_est) %>%
  summarise(Tiempo_m = mean(Tiempo_Medio),
            Tiempo_sd = sd(Tiempo_Medio))

comp_metodo = t_metodo %>%
  mutate(Método = reorder(metodo_est, Tiempo_m)) %>%
  ggplot(aes(x = metodo_est, y = Tiempo_m)) +
  geom_point(size = 0.8, color = "blue") +
  geom_errorbar(aes(ymin = Tiempo_m - Tiempo_sd,
                    ymax = Tiempo_m + Tiempo_sd),
                width = 0.2, color = "black") +
  labs(title = "Comparison of time-averaged clustering methods and number of strata",
       x = "Clustering method and number of strata",
       y = "Time average (s)") +
  theme_minimal() +
  coord_flip() +  # Para hacerlo horizontal (opcional)
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

print(comp_metodo)

# Guardar la imagen
ggsave(file = "time_comparison_method.png",
       plot = comp_metodo,
       device = "png",
       path = "./Results/5_Time",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)
