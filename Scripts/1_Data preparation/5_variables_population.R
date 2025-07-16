rm(list = ls())

library(tidyverse)
library(openxlsx)
library(rio)

# Apertura de las bases de datos de poblacion
censo_per = readRDS("./Data preparation/1_Data preparation/1_census_population.rds")

# Porcentaje de poblacion a nivel de UPM 
names(censo_per)
table(censo_per$lugar_hab, useNA = "ifany")
table(censo_per$nivel_instruccion, useNA = "ifany")
table(censo_per$hizo_sem_pasada, useNA = "ifany")
table(censo_per$si_no_trabajo, useNA = "ifany")

table(censo_per$hizo_sem_pasada, censo_per$si_no_trabajo, useNA = "ifany")
table(censo_per$hizo_sem_pasada, censo_per$trabaja_como, useNA = "ifany")

table(censo_per$sexo, useNA = "ifany")

table(censo_per$hijos_nacid_vivos, useNA = "ifany")

# analfabetismo
table(censo_per$sabe_leer, useNA = "ifany")
# asistencia a un establecimiento educativo
table(censo_per$asiste_estab_edu, useNA = "ifany")
# tenencia de seguro privado
table(censo_per$seguro_privado, useNA = "ifany")
# personas aseguradas a un tipo de seguro social
table(censo_per$seguridad_social, useNA = "ifany")
# educacion del jefe de hogar
table(censo_per$graesc, useNA = "ifany")

names(censo_per)

poblacion = censo_per %>%
  # personas con educacion superior
  mutate(educ_superior = ifelse((as.numeric(edad) >= 65 & nivel_instruccion %in% c("7","8","9","10")) | 
                                  (between(as.numeric(edad),25,64) & nivel_instruccion %in% c("9","10")), 1, 0),
         tot_educ_superior = ifelse(as.numeric(edad) >= 25, 1, 0),
         # personas con educacion media
         educ_media_num = ifelse(nivel_instruccion %in% c("4","5","6","7"), 1, 0),
         # personas con educacion baja
         educ_baja_num = ifelse(nivel_instruccion %in% c("1","2","3"), 1, 0),
         # personas en edad de trabajar (PET)
         edad = as.numeric(edad),
         pet = ifelse(as.numeric(edad) >= 15, 1, 0),
         # total hombres y mujeres en la PET
         tot_homb_pet = ifelse(pet == 1 & sexo == "1", 1, 0),
         tot_muje_pet = ifelse(pet == 1 & sexo == "2", 1, 0),
         # menores de 15 años
         menores = ifelse(as.numeric(edad) < 15, 1, 0),
         # poblacion economicamente activa (PEA)
         pea = ifelse(as.numeric(edad) >= 15 & (hizo_sem_pasada %in% c("1","2","3","4","5","6") |
                                                  si_no_trabajo == "1"), 1, 0),
         # personas ocupadas
         pers_ocupad = ifelse(pea == 1 & hizo_sem_pasada %in% c("1","2","3","4","5"), 1, 0),
         # hombres ocupados
         homb_ocupad = ifelse(pea == 1 & hizo_sem_pasada %in% c("1","2","3","4","5") & sexo == "1", 1, 0),
         # mujeres ocupadas
         muje_ocupad = ifelse(pea == 1 & hizo_sem_pasada %in% c("1","2","3","4","5") & sexo == "2", 1, 0),
         # no ocupada de 15 a 17 años
         no_ocupad_num_15_17 = ifelse(pers_ocupad != 1 & (as.numeric(edad) >= 15 & as.numeric(edad) <= 17), 1, 0),
         no_ocupad_den_15_17 = ifelse(as.numeric(edad) >= 15 & as.numeric(edad) <= 17, 1, 0),
         # personas desocupadas
         pers_desocupad = ifelse(pea == 1 & (hizo_sem_pasada %in% c("6") |
                                               si_no_trabajo == "1"), 1, 0),
         # hombres desocupados
         homb_desocupad = ifelse(pea == 1 & (hizo_sem_pasada %in% c("6") |
                                               si_no_trabajo == "1") & sexo == "1", 1, 0),
         # mujeres desocupadas
         muje_desocupad = ifelse(pea == 1 & (hizo_sem_pasada %in% c("6") |
                                               si_no_trabajo == "1") & sexo == "2", 1, 0),
         # total hombres y mujeres en la PEA
         tot_homb_pea = ifelse(pea == 1 & sexo == "1", 1, 0),
         tot_muje_pea = ifelse(pea == 1 & sexo == "2", 1, 0),
         # poblacion economicamente inactiva (PEI)
         pei = ifelse(as.numeric(edad) >= 15 & hizo_sem_pasada %in% c("7") &
                        si_no_trabajo %in% c("2","3","4","5","6","7"), 1, 0),
         # poblacion dependiente e independiente
         pers_depend = ifelse(as.numeric(edad) < 15 | as.numeric(edad) > 64, 1, 0),
         pers_independ = ifelse(between(as.numeric(edad), 15, 64), 1, 0),
         # total mujeres con hijos nacidos vivos
         hijos_nacid_vivos = as.numeric(hijos_nacid_vivos),
         muje_hijos_vivos = ifelse(as.numeric(edad) >= 15 & sexo == "2" & 
                                     as.numeric(hijos_nacid_vivos) <= 2, 1, 0),
         muje_hijos = ifelse(as.numeric(edad) >= 15 & sexo == "2", 1, 0),
         # Alfabetismo
         pob_alf_num = ifelse(as.numeric(edad) >= 15 & sabe_leer == "1", 1, 0),
         pob_alf_den = ifelse(as.numeric(edad) >= 15, 1, 0),
         # Alfabetismo: de 8 a 14 años de edad que saben leer o escribir
         pob_alf_num_8_14 = ifelse((as.numeric(edad) >= 8 & as.numeric(edad) <= 14) & sabe_leer == "1", 1, 0),
         pob_alf_den_8_14 = ifelse(as.numeric(edad) >= 8 & as.numeric(edad) <= 14, 1, 0),
         # asistencia escolar
         asist_escolar_num = ifelse((as.numeric(edad) >= 6 & as.numeric(edad) <= 19) & asiste_estab_edu == "1", 1, 0),
         asist_escolar_den = ifelse(as.numeric(edad) >= 6 & as.numeric(edad) <= 19, 1, 0),
         # tasa neta de asistencia de educación general básica (5 a 14 años)
         asist_egb_num_5_14 = ifelse((as.numeric(edad) >= 5 & as.numeric(edad) <= 14) & asiste_estab_edu == "1", 1, 0),
         asist_egb_den_5_14 = ifelse(as.numeric(edad) >= 5 & as.numeric(edad) <= 14, 1, 0),
         # Tasa neta de asistencia ajustada en bachillerato (15 a 17 años)
         asist_bac_num_15_17 = ifelse((as.numeric(edad) >= 15 & as.numeric(edad) <= 17) & asiste_estab_edu == "1", 1, 0),
         asist_bac_den_15_17 = ifelse(as.numeric(edad) >= 15 & as.numeric(edad) <= 17, 1, 0),
         # seguro privado
         seg_privado_num = ifelse(pea == 1 & seguro_privado == "1", 1, 0),
         seg_privado_den = ifelse(pea == 1 & seguro_privado %in% c("1","2"), 1, 0),
         # personas aseguradas a un tipo de seguro social
         seg_social_num = ifelse(pea == 1 & seguridad_social %in% c("1","2","3","4","5"), 1, 0),
         seg_social_den = ifelse(pea == 1, 1, 0),
         # educacion del jefe de hogar
         jefe = ifelse(parentesco == "1", 1, 0),
         edu_jefe = ifelse(jefe == 1, as.numeric(graesc)*jefe, 0)
         ) 

# Indicadores a nivel de UPM
upm_pob = poblacion %>%
  group_by(id_upm) %>%
  # personas con educacion superior
  summarise(educ_superior = sum(educ_superior, na.rm = T),
            tot_educ_superior = sum(tot_educ_superior, na.rm = T),
            # personas con educacion media
            educ_media_num = sum(educ_media_num, na.rm = T),
            educ_media_den = sum(!is.na(nivel_instruccion)),
            # personas con educacion baja
            educ_baja_num = sum(educ_baja_num, na.rm = T),
            educ_baja_den = sum(!is.na(nivel_instruccion)),
            # PET 
            pet = sum(pet, na.rm = T),
            # total hombres y mujeres en la PET
            tot_homb_pet = sum(tot_homb_pet, na.rm = T),
            tot_muje_pet = sum(tot_muje_pet, na.rm = T),
            # menores de 15 años
            menores = sum(menores, na.rm = T),
            # PEA
            pea = sum(pea, na.rm = T),
            # personas ocupadas
            pers_ocupad = sum(pers_ocupad, na.rm = T),
            # hombres ocupados
            homb_ocupad = sum(homb_ocupad, na.rm = T),
            # mujeres ocupadas
            muje_ocupad = sum(muje_ocupad, na.rm = T),
            # no ocupada de 15 a 17 años
            no_ocupad_num_15_17 = sum(no_ocupad_num_15_17, na.rm = T),
            no_ocupad_den_15_17 = sum(no_ocupad_den_15_17, na.rm = T),
            # personas desocupadas
            pers_desocupad = sum(pers_desocupad, na.rm = T),
            # hombres desocupados
            homb_desocupad = sum(homb_desocupad, na.rm = T),
            # mujeres desocupadas
            muje_desocupad = sum(muje_desocupad, na.rm = T),
            # total hombres y mujeres en la PEA
            tot_homb_pea = sum(tot_homb_pea, na.rm = T),
            tot_muje_pea = sum(tot_muje_pea, na.rm = T),
            # PEI
            pei = sum(pei, na.rm = T),
            # poblacion dependiente e independiente
            pers_depend = sum(pers_depend, na.rm = T),
            pers_independ = sum(pers_independ, na.rm = T),
            # total mujeres con hijos nacidos vivos
            muje_hijos_vivos = sum(muje_hijos_vivos, na.rm = T),
            muje_hijos = sum(muje_hijos, na.rm = T),
            # Analfabetismo
            pob_alf_num = sum(pob_alf_num, na.rm = T),
            pob_alf_den = sum(pob_alf_den, na.rm = T),
            # Alfabetismo: de 8 a 14 años de edad que saben leer o escribir
            pob_alf_num_8_14 = sum(pob_alf_num_8_14, na.rm = T),
            pob_alf_den_8_14 = sum(pob_alf_den_8_14, na.rm = T),
            # Asistencia escolar
            asist_escolar_num = sum(asist_escolar_num, na.rm = T),
            asist_escolar_den = sum(asist_escolar_den, na.rm = T),
            # tasa neta de asistencia de educación general básica (5 a 14 años)
            asist_egb_num_5_14 = sum(asist_egb_num_5_14, na.rm = T),
            asist_egb_den_5_14 = sum(asist_egb_den_5_14, na.rm = T),
            # Tasa neta de asistencia ajustada en bachillerato (15 a 17 años)
            asist_bac_num_15_17 = sum(asist_bac_num_15_17, na.rm = T),
            asist_bac_den_15_17 = sum(asist_bac_den_15_17, na.rm = T),
            # seguro privado
            seg_privado_num = sum(seg_privado_num, na.rm = T),
            seg_privado_den = sum(seg_privado_den, na.rm = T),
            # personas aseguradas a un tipo de seguro social
            seg_social_num = sum(seg_social_num, na.rm = T),
            seg_social_den = sum(seg_social_den, na.rm = T),
            # educacion del jefe de hogar
            edu_jefe_num = sum(edu_jefe, na.rm = T),
            edu_jefe_den = sum(jefe, na.rm = T),
            # grado promedio de escolaridad
            escol_media_num = sum(as.numeric(graesc), na.rm = T),
            escol_media_den = sum(!is.na(graesc))
            ) %>%
  # proporcion de personas con educacion superior
  mutate(p_educ_superior = round(educ_superior / tot_educ_superior * 100, 2),
         # proporcion de personas con educacion media
         p_educ_media = round(educ_media_num / educ_media_den * 100, 2),
         # proporcion de personas con educacion baja
         p_educ_baja = round(educ_baja_num / educ_baja_den * 100, 2),
         # proporcion de personas ocupadas
         p_pers_ocupad = round(pers_ocupad / pet * 100, 2),
         # proporcion de hombres ocupados
         p_homb_ocupad = round(homb_ocupad / tot_homb_pet * 100, 2),
         # proporcion de mujeres ocupadas
         p_muje_ocupad = round(muje_ocupad / tot_muje_pet * 100, 2),
         # no ocupada de 15 a 17 años
         p_no_ocupad_15_17 = round(no_ocupad_num_15_17 / no_ocupad_den_15_17 * 100, 2),
         # proporcion de personas no desocupadas (tasa de no desocupacion)
         p_pers_no_desocupad = round(100 - pers_desocupad / pea * 100, 2),
         # proporcion de hombres no desocupados
         p_homb_no_desocupad = round(100 - homb_desocupad / tot_homb_pea * 100, 2),
         # proporcion de mujeres no desocupadas
         p_muje_no_desocupad = round(100 - muje_desocupad / tot_muje_pea * 100, 2),
         # proporcion de personas con participacion en el mercado laboral
         p_pers_partlab = round(pea / pet * 100, 2),
         # proporcion de hombres con participacion en el mercado laboral
         p_homb_partlab = round(tot_homb_pea / tot_homb_pet * 100, 2),
         # proporcion de mujeres con participacion en el mercado laboral
         p_muje_partlab = round(tot_muje_pea / tot_muje_pet * 100, 2),
         # proporcion de personas dependientes
         p_pers_depend = pers_depend / pers_independ,
         # proporcion de personas que son generadoras de ingreso
         p_pers_geningreso = pers_depend / pers_ocupad,
         # proporcion de mujeres con dos o menos hijos nacidos vivos
         p_muje_hijos_vivos = round(muje_hijos_vivos / muje_hijos * 100, 2),
         # tasa de alfabetismo
         p_alfabetismo = round(pob_alf_num / pob_alf_den * 100, 2),
         # tasa de alfabetismo: de 8 a 14 años de edad que saben leer o escribir
         p_alfabetismo_8_14 = round(pob_alf_num_8_14 / pob_alf_den_8_14 * 100, 2),
         # tasa de asistencia escolar
         p_asist_escolar = round(asist_escolar_num / asist_escolar_den * 100, 2),
         # tasa neta de asistencia de educación general básica (5 a 14 años)
         p_asist_egb = round(asist_egb_num_5_14 / asist_egb_den_5_14 * 100, 2),
         # tasa neta de asistencia ajustada en bachillerato (15 a 17 años)
         p_asist_bac = round(asist_bac_num_15_17 / asist_bac_den_15_17 * 100, 2),
         # proporcion de personas que cuentan con seguro privado
         p_seguro_privado = round(seg_privado_num / seg_privado_den * 100, 2),
         # proporcion de personas aseguradas a un tipo de seguro social
         p_seguro_social = round(seg_social_num / seg_social_den * 100, 2),
         # educacion del jefe de hogar
         p_escol_jefe_media = edu_jefe_num / edu_jefe_den,
         # grado promedio de escolaridad
         p_escol_media = escol_media_num / escol_media_den
         )


# Verificacion de la poblacion total y mercado laboral
sum(poblacion$menores)
sum(poblacion$pet)

sum(poblacion$menores) + sum(poblacion$pet) == dim(censo_per)[1] # pob total (PT)

sum(poblacion$pea)
sum(poblacion$pei)

sum(poblacion$tot_homb_pet) + sum(poblacion$tot_muje_pet) == sum(poblacion$pet) # PET
sum(poblacion$pea) + sum(poblacion$pei) == sum(poblacion$pet) # PET

sum(poblacion$pers_ocupad, na.rm = T) + sum(poblacion$pers_desocupad, na.rm = T) == sum(poblacion$pea, na.rm = T) # PEA

sum(poblacion$menores, na.rm = T) + sum(poblacion$pea, na.rm = T) + sum(poblacion$pei, na.rm = T) == dim(censo_per)[1] # PT

sum(poblacion$homb_ocupad)
sum(poblacion$muje_ocupad)
sum(poblacion$homb_desocupad, na.rm = T)
sum(poblacion$muje_desocupad, na.rm = T)

sum(poblacion$menores, na.rm = T) + sum(poblacion$homb_ocupad, na.rm = T) + sum(poblacion$muje_ocupad, na.rm = T) +
  sum(poblacion$homb_desocupad, na.rm = T) + sum(poblacion$muje_desocupad, na.rm = T) + sum(poblacion$pei, na.rm = T) == 
  dim(censo_per)[1] # PT

# Verificacion de la razon de dependencia
sum(poblacion$pers_depend) + sum(poblacion$pers_independ) == dim(censo_per)[1] # PT


# Analisis descriptivo
summary(upm_pob$p_educ_superior) # NA
summary(upm_pob$p_educ_media) 
summary(upm_pob$p_educ_baja)
summary(upm_pob$p_pers_ocupad) # NA
summary(upm_pob$p_homb_ocupad) # NA
summary(upm_pob$p_muje_ocupad) # NA
summary(upm_pob$p_no_ocupad_15_17) # NA
summary(upm_pob$p_pers_no_desocupad) # NA
summary(upm_pob$p_homb_no_desocupad) # NA
summary(upm_pob$p_muje_no_desocupad) # NA
summary(upm_pob$p_pers_partlab) # NA
summary(upm_pob$p_homb_partlab) # NA
summary(upm_pob$p_muje_partlab) # NA
summary(upm_pob$p_pers_depend) # Inf
summary(upm_pob$p_pers_geningreso) # NA
summary(upm_pob$p_muje_hijos_vivos) # NA
summary(upm_pob$p_alfabetismo) # NA
summary(upm_pob$p_alfabetismo_8_14) # NA
summary(upm_pob$p_asist_escolar) # NA
summary(upm_pob$p_asist_egb) # NA
summary(upm_pob$p_asist_bac) # NA
summary(upm_pob$p_seguro_privado) # NA
summary(upm_pob$p_seguro_social) # NA
summary(upm_pob$p_escol_jefe_media) # NA
summary(upm_pob$p_escol_media) # NA


perd = filter(upm_pob, is.na(p_pers_geningreso))
perd = filter(upm_pob, is.infinite(p_pers_depend))
perd = filter(upm_pob, p_pers_geningreso > 1)

apply(is.na(upm_pob), 2, sum) # valores perdidos en las variables

# Funcion para hacer ceros los valores perdidos o los valores infinitos
ceros = function(x) {
  x = ifelse(is.na(x) | is.infinite(x), 0, x)
}
# aplicacion de la funcion
upm_pob_f = upm_pob
upm_pob_f[,c(46:dim(upm_pob)[2])] = as.data.frame(
  apply(
  upm_pob_f[,c(46:dim(upm_pob)[2])], 2, ceros)
)

str(upm_pob_f)

# Analisis descriptivo
summary(upm_pob_f$p_educ_superior) 
summary(upm_pob_f$p_educ_media) 
summary(upm_pob_f$p_educ_baja) 
summary(upm_pob_f$p_pers_ocupad) 
summary(upm_pob_f$p_homb_ocupad) 
summary(upm_pob_f$p_muje_ocupad)
summary(upm_pob_f$p_no_ocupad_15_17) 
summary(upm_pob_f$p_pers_no_desocupad) 
summary(upm_pob_f$p_homb_no_desocupad) 
summary(upm_pob_f$p_muje_no_desocupad) 
summary(upm_pob_f$p_pers_partlab)
summary(upm_pob_f$p_homb_partlab)
summary(upm_pob_f$p_muje_partlab)
summary(upm_pob_f$p_pers_depend) # maximo es mayor a 1
summary(upm_pob_f$p_pers_geningreso) # maximo es mayor a 1
summary(upm_pob_f$p_muje_hijos_vivos)
summary(upm_pob_f$p_alfabetismo)
summary(upm_pob_f$p_alfabetismo_8_14) 
summary(upm_pob_f$p_asist_escolar)
summary(upm_pob_f$p_asist_egb)
summary(upm_pob_f$p_asist_bac)
summary(upm_pob_f$p_seguro_privado)
summary(upm_pob_f$p_seguro_social)
summary(upm_pob_f$p_escol_jefe_media) # maximo es mayor a 1
summary(upm_pob_f$p_escol_media) # maximo es mayor a 1


# Escalar minimo-maximo las variables "prop_pers_depend", "prop_pers_geningreso"
# "prop_edu_jefe_media", "prop_escol_media"
# debido a que el valor maximo es mayor a 1
upm_pob_f = upm_pob_f %>%
  mutate(p_pers_depend = (p_pers_depend - min(p_pers_depend, na.rm = T)) /
           (max(p_pers_depend) - min(p_pers_depend)),
         p_pers_geningreso = (p_pers_geningreso - min(p_pers_geningreso)) /
           (max(p_pers_geningreso) - min(p_pers_geningreso)),
         p_escol_jefe_media = (p_escol_jefe_media - min(p_escol_jefe_media)) /
           (max(p_escol_jefe_media) - min(p_escol_jefe_media)),
         p_escol_media = (p_escol_media - min(p_escol_media)) /
           (max(p_escol_media) - min(p_escol_media))
         ) %>%
  mutate(p_pers_depend = round(100 - p_pers_depend * 100, 2),
         p_pers_geningreso = round(p_pers_geningreso * 100, 2),
         p_escol_jefe_media = round(p_escol_jefe_media * 100, 2),
         p_escol_media = round(p_escol_media * 100, 2)
         )

summary(upm_pob_f$p_pers_depend)
summary(upm_pob_f$p_pers_geningreso)
summary(upm_pob_f$p_escol_jefe_media)
summary(upm_pob_f$p_escol_media)

# Guardar las bases de datos
saveRDS(poblacion, "./Data preparation/1_Data preparation/4_population.rds")
saveRDS(upm_pob_f, "./Data preparation/1_Data preparation/4_PSU_population.rds")
