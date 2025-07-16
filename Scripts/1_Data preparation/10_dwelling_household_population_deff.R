rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Preparacion de las bases de datos para calcular el deff
# A nivel de censo se enlazan los resultados y se obtiene los deff

## Hogares
hogares_deff = readRDS("./Data preparation/1_Data preparation/6_household.rds")
apply(is.na(hogares_deff), 2, sum)

hogares_deff = hogares_deff %>%
  rename(id_sector = id_upm) %>%
  select(id_sector, starts_with("dummy_"))

names(hogares_deff) = str_replace(names(hogares_deff),"dummy", "h")
names(hogares_deff)

## Viviendas
viviendas_deff = readRDS("./Data preparation/1_Data preparation/5_dwelling.rds")
apply(is.na(viviendas_deff), 2, sum)

viviendas_deff = viviendas_deff %>%
  rename(id_sector = id_upm) %>%
  mutate(hacinamiento = ifelse(hacinamiento == 0, 1, 0)) %>%
  select(id_sector, 
         v_no_hacin = hacinamiento,
         v_agua = agua_proviene,
         v_muros = muros_con_bienes,
         v_materialidad = materialidad,
         v_proviene_luz = dummy_proviene_luz,
         v_recibe_agua = dummy_recibe_agua,
         v_serv_higienico = dummy_serv_higienico,
         v_eliminacion_basura = dummy_eliminacion_basura,
         v_dispone_luz = dispone_luz_num)

names(viviendas_deff)

## Personas
personas_deff = readRDS("./Data preparation/1_Data preparation/4_population.rds")
apply(is.na(personas_deff), 2, sum)

personas_deff = personas_deff %>%
  rename(id_sector = id_upm) %>%
  mutate(p_educ_superior = ifelse(educ_superior == 1, 1,
                                  ifelse(is.na(nivel_instruccion), NA, 0)),
         p_pers_ocupad = ifelse(pers_ocupad == 1, 1, 
                                ifelse(pers_ocupad == 0 & pet == 1, 0, NA)),
         p_homb_ocupad = ifelse(homb_ocupad == 1, 1, 
                                ifelse(homb_ocupad == 0 & pet == 1, 0, NA)),
         p_muje_ocupad = ifelse(muje_ocupad == 1, 1, 
                                ifelse(muje_ocupad == 0 & pet == 1, 0, NA)),
         p_no_ocupad_15_17 = ifelse(no_ocupad_num_15_17 == 1, 1, 
                                    ifelse(no_ocupad_num_15_17 == 0 & no_ocupad_den_15_17 == 1, 0, NA)),
         p_pea = ifelse(pea == 1, 1, NA),
         p_pers_no_desocupad = ifelse(pers_ocupad == 1, 1,
                                      ifelse(pers_ocupad == 0 & pea == 1, 0, NA)),
         p_homb_no_desocupad = ifelse(homb_ocupad == 1, 1,
                                      ifelse(homb_ocupad == 0 & pea == 1, 0, NA)),
         p_muje_no_desocupad = ifelse(muje_ocupad == 1, 1,
                                      ifelse(muje_ocupad == 0 & pea == 1, 0, NA)),
         p_pers_partlab = ifelse(pea == 1, 1,
                                 ifelse(pea == 0 & pet == 1, 0, NA)),
         p_homb_partlab = ifelse(tot_homb_pea == 1, 1,
                                 ifelse(tot_homb_pea == 0 & tot_homb_pet == 1, 0, NA)),
         p_muje_partlab = ifelse(tot_muje_pea == 1, 1,
                                 ifelse(tot_muje_pea == 0 & tot_muje_pet == 1, 0, NA)),
         p_muje_hijos_vivos = ifelse(muje_hijos_vivos == 1, 1,
                                     ifelse(muje_hijos_vivos == 0 & muje_hijos == 1, 0, NA)),
         p_alfabetismo = ifelse(pob_alf_num == 1, 1,
                                ifelse(pob_alf_num == 0 & pob_alf_den == 1, 0, NA)),
         p_alfabetismo_8_14 =  ifelse(pob_alf_num_8_14 == 1, 1,
                                      ifelse(pob_alf_num_8_14 == 0 & pob_alf_den_8_14 == 1, 0, NA)),
         p_asist_escolar = ifelse(asist_escolar_num == 1, 1,
                                  ifelse(asist_escolar_num == 0 & asist_escolar_den == 1, 0, NA)),
         p_asist_egb = ifelse(asist_egb_num_5_14 == 1, 1,
                              ifelse(asist_egb_num_5_14 == 0 & asist_egb_den_5_14 == 1, 0, NA)),
         p_asist_bac = ifelse(asist_bac_num_15_17 == 1, 1,
                              ifelse(asist_bac_num_15_17 == 0 & asist_bac_den_15_17 == 1, 0, NA)),
         p_seguro_privado = ifelse(seg_privado_num == 1, 1,
                                   ifelse(seg_privado_num == 0 & seg_privado_den == 1, 0, NA)),
         p_seguro_social = ifelse(seg_social_num == 1, 1,
                                  ifelse(seg_social_num == 0 & seg_social_den == 1, 0, NA)),
  ) %>%
  group_by(id_hog) %>%
  mutate(p_escol_jefe_media = sum(edu_jefe, na.rm = T)) %>%
  ungroup() %>%
  group_by(id_sector) %>%
  mutate(p_escol_media = sum(as.numeric(graesc), na.rm = T)/sum(!is.na(graesc)),
         p_pers_depend = sum(pers_independ, na.rm = T) / sum(pers_depend, na.rm = T),
         p_pers_depend = ifelse(is.infinite(p_pers_depend), 1, p_pers_depend)) %>%
  ungroup()

names(personas_deff)

# Guardar los conjuntos de datos
saveRDS(personas_deff, "./Data preparation/1_Data preparation/4_population_deff.rds")
saveRDS(viviendas_deff, "./Data preparation/1_Data preparation/5_dwelling_deff.rds")
saveRDS(hogares_deff, "./Data preparation/1_Data preparation/6_household_deff.rds")
