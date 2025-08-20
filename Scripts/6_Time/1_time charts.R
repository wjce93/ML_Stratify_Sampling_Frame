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

t_metodo$metodo_est <- gsub("Agglomerative \\(k=(\\d+)\\)", "AC (k=\\1)", t_metodo$metodo_est)
t_metodo$metodo_est <- gsub("MiniBatchKMeans \\(k=(\\d+)\\)", "MBKM (k=\\1)", t_metodo$metodo_est)
t_metodo$metodo_est <- gsub("KMeans \\(k=(\\d+)\\)", "KM (k=\\1)", t_metodo$metodo_est)
t_metodo$metodo_est <- gsub("Spectral \\(k=(\\d+)\\)", "SC (k=\\1)", t_metodo$metodo_est)

comp_metodo = t_metodo %>%
  mutate(Método = reorder(metodo_est, Tiempo_m)) %>%
  ggplot(aes(x = metodo_est, y = Tiempo_m)) +
  geom_point(size = 1.5, color = "blue") +
  geom_errorbar(aes(ymin = Tiempo_m - Tiempo_sd,
                    ymax = Tiempo_m + Tiempo_sd),
                width = 0.2, color = "black") +
  labs(title = "Comparison of time-averaged clustering methods and number of strata",
       x = "Clustering method and number of strata",
       y = "Time average (s)") +
  coord_flip() +  # Para hacerlo horizontal (opcional)
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  # theme_minimal() 

print(comp_metodo)

# Guardar la imagen
ggsave(file = "time_comparison_method.png",
       plot = comp_metodo,
       device = "png",
       path = "./Results/5_Time",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)


# Graficos de caja y bigote del tiempo (en segundos) entre las representaciones y los metodos
# de clustering

gra_execution_time = datos %>%
  select(metodo_u, representacion_u, Tiempo_Medio) %>%
  ggplot(aes(x = representacion_u, y = Tiempo_Medio)) + 
  geom_boxplot(aes(fill = metodo_u)) + 
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  scale_fill_manual(values = c("gray20","dodgerblue4","darkgoldenrod2","darkslategray4","darksalmon","darkolivegreen4"),
                    labels = c("AC", "BIRCH", "GMM", "KM", "MBKM", "SC")) +
  labs(fill = "Clustering method") +
  ggtitle("Average execution time by representation and clustering method (cross validation)") +
  xlab("Representation") + 
  ylab("Time (seconds)") +
  scale_x_discrete(labels = c("Autoencoder", "Kernel PCA", "Original", "PCA")) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18)) 

print(gra_execution_time)

# Guardar la imagen
ggsave(file = "Execution_time.png",
       plot = gra_execution_time,
       device = "png",
       path = "./Results/5_Time",
       scale = 0.25, width = 1920, height = 1080, units = "mm",
       dpi = 300,
       limitsize = F)
