# Estratificacion univariada
metodos_univ <- function(y_trans, H){
  
  #Creo una base donde se van a quedar las estratificaciones a nivel de sector censal
  univariado <- data.frame(id_sector= indicadores$id_sector) 
  
  #guardo la primera componente
  Factor_Posit <- y_trans
  
  #Percentiles
  estratif_percentil  <- quantile(Factor_Posit,
                                  probs = seq(0, 1, by = 1 / H),)
  univariado$percentil <- cut(
    Factor_Posit,
    breaks = estratif_percentil,
    include.lowest = T,
    right = F,
    label = 1:H
  )
  
  #Dalenius
  estratif_danelniusH3 <-
    strata.cumrootf(
      x = Factor_Posit , #componente principal
      Ls = H,
      CV = 0.03,
      alloc = c(0.5, 0, 0.5) #afijacion proporcional
    )
  univariado$Dalenius <- estratif_danelniusH3$stratumID
  
  # LH_Kozak
  estratif_LH_KozakH3 <-
    strata.LH(
      x = Factor_Posit ,
      Ls = H,
      CV = 0.03,
      alloc = c(0.5, 0, 0.5),
      algo = "Kozak"
    )
  univariado$LH_Kozak <- estratif_LH_KozakH3$stratumID
  
  #Geometrico
  estratif_geoH3 <- strata.geo(
    x = Factor_Posit ,
    Ls = H,
    CV = 0.03,
    alloc = c(0.5, 0, 0.5)
  )
  univariado$geometric <- estratif_geoH3$stratumID
  
  return(univariado)
}

# Funciones para calcular el DEFF
deff_estratificado <- function(datos,
                               txt_variable_evaluacion,
                               txt_estrato) {
  datos <- datos |> filter(!is.na(get(txt_variable_evaluacion))) 
  modelo <-
    lm(datos[[txt_variable_evaluacion]]  ~ datos[[txt_estrato]] )
  resumen <- summary(modelo)
  deff <- 1 - resumen$r.squared
  return(deff)
}

deff_datas <- function(data_censo, metodo, df_univariado, variables_estratificacion){
  
  variable_evaluacion <- intersect(colnames(data_censo), variables_estratificacion)
  
  df <- data_censo |>  
    select(c(id_sector, all_of(variable_evaluacion))) |> 
    left_join(df_univariado %>%select("id_sector", all_of(metodo)),
              by = "id_sector")
  
  aux_deff <- as.numeric(length(variable_evaluacion))
  for (i in 1:length(variable_evaluacion)) {
    aux_deff[i] <-
      deff_estratificado(df, variable_evaluacion[i], metodo)
  }
  names(aux_deff) <- variable_evaluacion
  
  rm(data_censo, df)
  
  return(aux_deff)
  
}