"
¿Te gustaría que escribamos el código para calcular y evaluar el primer modelo 
de referencia (Persistencia con su antigüedad) sobre el conjunto de 
reserva (2023-2024) para obtener nuestro primer MAE y RMSE de comparación?
"


library(dplyr)

# ==============================================================================
# 1. BASELINE 1: PERSISTENCIA (Última observación disponible)
# ==============================================================================

eval_persistencia <- datos_prueba %>%
  filter(!is.na(pm25_last_obs)) %>%
  mutate(
    error_abs = abs(target_pm25_t1 - pm25_last_obs),
    error_cuad = (target_pm25_t1 - pm25_last_obs)^2
  )

mae_persistencia <- mean(eval_persistencia$error_abs)

rmse_persistencia <- sqrt(mean(eval_persistencia$error_cuad))


# ==============================================================================
# 2. BASELINE 2: PATRÓN ESTACIONAL (Entrenado únicamente en 2019-2022)
# ==============================================================================

# Se calcula la media por estación y mes en el conjunto de entrenamiento

patron_estacional <- datos_entrenamiento %>%
  group_by(County.Code, Site.Num, mes) %>%
  summarise(
    pred_estacional = mean(pm25_t, na.rm = TRUE),
    .groups = "drop"
  )

# Se evalúa en el conjunto de prueba (2023-2024)

eval_estacional <- datos_prueba %>%
  left_join(patron_estacional, by = c("County.Code", "Site.Num", "mes")) %>%
  filter(!is.na(pred_estacional)) %>%
  mutate(
    error_abs = abs(target_pm25_t1 - pred_estacional),
    error_cuad = (target_pm25_t1 - pred_estacional)^2
  )

mae_estacional <- mean(eval_estacional$error_abs)

rmse_estacional <- sqrt(mean(eval_estacional$error_cuad))


# ==============================================================================
# 3. TABLA RESUMEN DE REFERENCIA (Global)
# ==============================================================================

resumen_baselines <- tibble(
  Modelo = c("Persistencia", "Patrón Estacional (Estación - Mes)"),
  MAE = c(mae_persistencia, mae_estacional),
  RMSE = c(rmse_persistencia, rmse_estacional)
)

print(resumen_baselines)



# 1. Errores por estación

mae_por_estacion <- eval_persistencia %>%
  group_by(County.Code, Site.Num) %>%
  summarise(
    mae = mean(error_abs),
    rmse = sqrt(mean(error_cuad)),
    dias_evaluados = n(),
    .groups = "drop"
  )


# 2. Errores por época del año (mes/estación)

mae_por_mes <- eval_persistencia %>%
  group_by(mes) %>%
  summarise(
    mae = mean(error_abs),
    rmse = sqrt(mean(error_cuad)),
    .groups = "drop"
  )


# 3. Episodios de alta concentración (definidos en desarrollo, e.g. PM2.5 > 35 µg/m3)

mae_alta_contaminacion <- eval_persistencia %>%
  mutate(episodio_alto = target_pm25_t1 > 35) %>%
  group_by(episodio_alto) %>%
  summarise(
    mae = mean(error_abs),
    rmse = sqrt(mean(error_cuad)),
    casos = n()
  )