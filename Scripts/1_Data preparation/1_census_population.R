rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Apertura de la base de datos de personas
censo_per = import("./Data/CPV2010_Spss_Poblacion.sav") %>%
  # rename_all(tolower) %>%
  # variables de ubicacion geografica
  select(I01, I02, I03, I04, I05, I09, I10, P00, URP, 
         # caracteristicas personales como sexo y edad
         P01, P02, P03,
         # seguro privado
         P07,
         # discapacidad
         P08,
         # lugar de residencia habitual
         P12L, P12,
         # autoidentificacion etnica
         P16,
         # alfabetizacion y uso de dispositivos
         P19, P20T, P20I, P20C,
         # enseñanza y nivel de instruccion
         P21, P22, P23, P24, P25,
         # trabajo, ocupacion y rama de actividad
         P27, P28, P291, P31, P32,
         # estado civil
         P34,
         # seguridad social aporta o es afiliado
         P35,
         # hijos nacidos vivos
         P36, P36H, P36M, P38,
         # algunas variables recodificadas
         TIPOACT, RAMCT, GRUOCU, GRAESC, GRAESCSA
         ) %>%
  # renombrar algunas variables
  rename(prov = I01, cant = I02, parroq = I03, zona = I04, sector =I05, 
         vivienda = I09, hogar = I10, persona = P00, area = URP,
         sexo = P01, parentesco = P02, edad = P03,
         seguro_privado = P07,
         discapacidad = P08,
         lugar_hab = P12L, parroq_hab = P12,
         etnia = P16,
         sabe_leer = P19, uso_celular = P20T, uso_internet = P20I, uso_compu = P20C,
         asiste_estab_edu = P21, tipo_estab_edu = P22, nivel_instruccion = P23, 
         año_aprobado = P24, titulo_superior = P25,
         hizo_sem_pasada = P27, si_no_trabajo = P28, rama_actividad = P291, 
         trabaja_como = P31, horas_trabajadas = P32,
         seguridad_social = P35,
         estado_civil = P34,
         pers_nacid_vivos = P36, hijos_nacid_vivos = P36H,
         hijas_nacid_vivos = P36M, edad_primer_hijo = P38
         ) %>%
  rename_all(tolower) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(prov = str_pad(prov, 2, "left", "0"),
         cant = str_pad(cant, 2, "left", "0"),
         parroq = str_pad(parroq, 2, "left", "0"),
         zona = str_pad(zona, 3, "left", "0"),
         sector = str_pad(sector, 3, "left", "0"),
         vivienda = str_pad(vivienda, 3, "left", "0"),
         hogar = as.character(hogar),
         persona = str_pad(persona, 4, "left", "0"),
         parroq_hab = str_pad(parroq_hab, 6, "left", "0")
         )

# Guardar la base de datos resultante
saveRDS(censo_per, "./Data/CPV2010_Spss_Poblacion.rds")
