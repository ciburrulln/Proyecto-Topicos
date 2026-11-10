source("limpieza.R")


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

# Solo para un bloque de tiempo #
#### 2019-2020-2021 vs 2022 ###

entrenamiento <- pm25_diario %>% filter(Date.Local >= "2019-01-01" & Date.Local <= "2021-12-31")
validacion   <- pm25_diario %>% filter(Date.Local >= "2022-01-01" & Date.Local <= "2022-12-31")

# Métricas de usando solo la referencia (última concentración observada)
# Evaluación en entrenamiento
metricas_train <- calcular_metricas_referencia(entrenamiento)

# Evaluación en validación
metricas_val <- calcular_metricas_referencia(validacion)

# Comparar resultados
bind_rows(
  Entrenamiento = metricas_train,
  Validacion = metricas_val,
  .id = "Conjunto"
)



# Estudiar cómo se comporta según la antigüedad de la referencia 

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

# Aplicar a entrenamiento y validación
resultados_antiguedad <- bind_rows(
  evaluar_antiguedad(entrenamiento) %>%
    mutate(Conjunto = "Entrenamiento"),
  
  evaluar_antiguedad(validacion) %>%
    mutate(Conjunto = "Validación")
)

print(resultados_antiguedad)

