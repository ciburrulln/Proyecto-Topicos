
library(dplyr)
library(lubridate)

# Uso de IA #
# La IA apoyó la creación de funciones para calcular
# las métricas del patrón estacional en los tres
# bloques temporales.

#### REFERENCIA ESTACIONAL ####

predicciones_estacionales <- bind_rows(
  lapply(names(particiones), function(nombre) {
    
    entrenamiento <- particiones[[nombre]]$entrenamiento
    validacion <- particiones[[nombre]]$validacion
    
    # Calcular promedio histórico por estación y mes,
    # utilizando únicamente entrenamiento
    
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
    
    # Asignar las predicciones a entrenamiento
    entrenamiento <- entrenamiento %>%
      mutate(
        mes_objetivo = month(Date.Local + 1)
      ) %>%
      left_join(
        patron_estacional,
        by = c("Station", "mes_objetivo")
      )
    
    # Asignar las predicciones a validación
    validacion <- validacion %>%
      mutate(
        mes_objetivo = month(Date.Local + 1)
      ) %>%
      left_join(
        patron_estacional,
        by = c("Station", "mes_objetivo")
      )
    
    # Juntar ambos conjuntos
    bind_rows(
      Entrenamiento = entrenamiento,
      Validacion = validacion,
      .id = "Conjunto"
    ) %>%
      mutate(Bloque = nombre)
  })
)


#### MÉTRICAS DE ENTRENAMIENTO Y VALIDACIÓN ####

resultados_estacional <- predicciones_estacionales %>%
  filter(
    !is.na(pm25_futuro),
    !is.na(pred_estacional)
  ) %>%
  group_by(Bloque, Conjunto) %>%
  summarise(
    n = n(),
    MAE = mean(abs(pm25_futuro - pred_estacional)),
    MSE = mean((pm25_futuro - pred_estacional)^2),
    RMSE = sqrt(MSE),
    .groups = "drop"
  )

print(resultados_estacional)


#### COMPARACIÓN DE LAS DOS REFERENCIAS ####

# Comparamos solamente validación y utilizamos
# exactamente las mismas observaciones

comparacion_referencias <- predicciones_estacionales %>%
  filter(
    Conjunto == "Validacion",
    !is.na(pm25_futuro),
    !is.na(ultima_conc),
    !is.na(pred_estacional)
  ) %>%
  group_by(Bloque) %>%
  summarise(
    n = n(),
    
    MAE_ultima_conc =
      mean(abs(pm25_futuro - ultima_conc)),
    
    MAE_estacional =
      mean(abs(pm25_futuro - pred_estacional)),
    
    RMSE_ultima_conc =
      sqrt(mean((pm25_futuro - ultima_conc)^2)),
    
    RMSE_estacional =
      sqrt(mean((pm25_futuro - pred_estacional)^2)),
    
    .groups = "drop"
  )

print(comparacion_referencias)

