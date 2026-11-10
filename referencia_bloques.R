
library(dplyr)

# Uso de IA #
# La IA generó las funciones para calcular los resultados para
# los 3 bloques de tiempo 

#### FUNCIONES ####

# Función para calcular métricas de referencia
calcular_metricas_referencia <- function(datos) {
  
  datos %>%
    filter(
      !is.na(pm25_futuro),
      !is.na(ultima_conc)
    ) %>%
    summarise(
      n = n(),
      MAE = mean(abs(pm25_futuro - ultima_conc)),
      MSE = mean((pm25_futuro - ultima_conc)^2),
      RMSE = sqrt(MSE)
    )
}


# Función para evaluar según antigüedad
evaluar_antiguedad <- function(datos) {
  
  datos %>%
    filter(
      !is.na(pm25_futuro),
      !is.na(ultima_conc),
      !is.na(antiguedad_dias)
    ) %>%
    mutate(
      grupo_antiguedad = case_when(
        antiguedad_dias == 0 ~ "0 días",
        antiguedad_dias == 1 ~ "1 día",
        antiguedad_dias <= 3 ~ "2-3 días",
        TRUE ~ "4 o más días"
      )
    ) %>%
    group_by(grupo_antiguedad) %>%
    summarise(
      n = n(),
      MAE = mean(abs(pm25_futuro - ultima_conc)),
      MSE = mean((pm25_futuro - ultima_conc)^2),
      RMSE = sqrt(MSE),
      .groups = "drop"
    )
}


#### DEFINIR LOS TRES BLOQUES TEMPORALES ####

bloques <- list(
  
  bloque_1 = list(
    inicio_train = as.Date("2019-01-01"),
    inicio_val = as.Date("2020-01-01"),
    fin_val = as.Date("2020-12-31")
  ),
  
  bloque_2 = list(
    inicio_train = as.Date("2019-01-01"),
    inicio_val = as.Date("2021-01-01"),
    fin_val = as.Date("2021-12-31")
  ),
  
  bloque_3 = list(
    inicio_train = as.Date("2019-01-01"),
    inicio_val = as.Date("2022-01-01"),
    fin_val = as.Date("2022-12-31")
  )
)


#### CREAR ENTRENAMIENTO Y VALIDACIÓN ####

particiones <- lapply(bloques, function(b) {
  
  entrenamiento <- pm25_diario %>%
    filter(
      Date.Local >= b$inicio_train,
      Date.Local + 1 < b$inicio_val
    )
  
  validacion <- pm25_diario %>%
    filter(
      Date.Local + 1 >= b$inicio_val,
      Date.Local + 1 <= b$fin_val
    )
  
  list(
    entrenamiento = entrenamiento,
    validacion = validacion
  )
})


#### VERIFICAR PARTICIONES ####

comprobacion <- bind_rows(
  lapply(names(particiones), function(nombre) {
    
    datos <- particiones[[nombre]]$validacion
    
    tibble(
      Bloque = nombre,
      primera_fecha_predicha = min(datos$Date.Local + 1),
      ultima_fecha_predicha = max(datos$Date.Local + 1),
      n = nrow(datos)
    )
  })
)

print(comprobacion)



#### EVALUACIÓN GLOBAL DE LA REFERENCIA ####

resultados_globales <- bind_rows(
  lapply(names(particiones), function(nombre) {
    
    entrenamiento <- particiones[[nombre]]$entrenamiento
    validacion <- particiones[[nombre]]$validacion
    
    bind_rows(
      Entrenamiento = calcular_metricas_referencia(entrenamiento),
      Validacion = calcular_metricas_referencia(validacion),
      .id = "Conjunto"
    ) %>%
      mutate(Bloque = nombre)
    
  })
)

print(resultados_globales)


#### EVALUACIÓN SEGÚN ANTIGÜEDAD ####

resultados_antiguedad <- bind_rows(
  lapply(names(particiones), function(nombre) {
    
    entrenamiento <- particiones[[nombre]]$entrenamiento
    validacion <- particiones[[nombre]]$validacion
    
    bind_rows(
      Entrenamiento = evaluar_antiguedad(entrenamiento),
      Validacion = evaluar_antiguedad(validacion),
      .id = "Conjunto"
    ) %>%
      mutate(Bloque = nombre)
    
  })
)

