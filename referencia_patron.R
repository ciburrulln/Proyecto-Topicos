
library(dplyr)
library(lubridate)

# Uso de IA #
# La IA apoyó la creación de funciones para calcular
# el patrón estacional en los tres bloques temporales.

#### REFERENCIA ESTACIONAL ####

# Calcular los promedios históricos por estación y mes,
# utilizando únicamente los datos de entrenamiento.

predicciones_estacionales <- bind_rows(
  lapply(names(particiones), function(nombre) {
    
    entrenamiento <- particiones[[nombre]]$entrenamiento
    validacion <- particiones[[nombre]]$validacion
    
    # Calcular el promedio histórico de PM2.5
    # por estación y mes del día objetivo
    
    patron_estacional <- entrenamiento %>%
      mutate(
        mes_objetivo = month(Date.Local + 1)
      ) %>%
      filter(!is.na(pm25_futuro)) %>%
      group_by(Station, mes_objetivo) %>%
      summarise(
        pred_estacional = mean(pm25_futuro),
        n_historico = n(),
        .groups = "drop"
      )
    
    # Asignar a cada observación de validación
    # el promedio histórico correspondiente
    
    validacion %>%
      mutate(
        mes_objetivo = month(Date.Local + 1)
      ) %>%
      left_join(
        patron_estacional,
        by = c("Station", "mes_objetivo")
      ) %>%
      mutate(Bloque = nombre)
  })
)


#### EVALUACIÓN GLOBAL DEL PATRÓN ESTACIONAL ####

resultados_estacional <- predicciones_estacionales %>%
  filter(
    !is.na(pm25_futuro),
    !is.na(pred_estacional)
  ) %>%
  group_by(Bloque) %>%
  summarise(
    n = n(),
    MAE = mean(abs(pm25_futuro - pred_estacional)),
    MSE = mean((pm25_futuro - pred_estacional)^2),
    RMSE = sqrt(MSE),
    .groups = "drop"
  )

print(resultados_estacional)


#### COMPARACIÓN DE LAS DOS REFERENCIAS ####

# Evaluar ambas referencias sobre exactamente
# las mismas observaciones de validación.

comparacion_referencias <- predicciones_estacionales %>%
  filter(
    !is.na(pm25_futuro),
    !is.na(ultima_conc),
    !is.na(pred_estacional)
  ) %>%
  group_by(Bloque) %>%
  summarise(
    n = n(),
    
    MAE_ultima_conc = mean(abs(pm25_futuro - ultima_conc)),
    MAE_estacional = mean(abs(pm25_futuro - pred_estacional)),
    
    RMSE_ultima_conc = sqrt(mean((pm25_futuro - ultima_conc)^2)),
    RMSE_estacional = sqrt(mean((pm25_futuro - pred_estacional)^2)),
    
    .groups = "drop"
  )

print(comparacion_referencias)
