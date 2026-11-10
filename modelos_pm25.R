
library(dplyr)
library(lubridate)
library(glmnet)
library(rpart)
library(ranger)

stopifnot(exists("pm25_diario"),
          exists("particiones"),
          exists("predicciones_estacionales"))

set.seed(2221268)
pm25_diario <- pm25_diario %>%
  mutate(Date.Local = as.Date(Date.Local))

niveles_estacion <- pm25_diario %>%
  pull(Station) %>% as.character() %>% unique() %>% sort()

variables_numericas <- c("pm25_hoy", "pm25_lag1", "pm25_lag7",
                         "ultima_conc", "antiguedad_dias")

# La fecha solo define el mes del dia a predecir: no es una entrada directa.
base_modelo <- function(datos) {
  datos %>%
    transmute(
      pm25_futuro = as.numeric(pm25_futuro),
      Station = factor(as.character(Station), levels = niveles_estacion),
      mes_futuro = factor(as.character(month(as.Date(Date.Local) + 1)),
                          levels = as.character(1:12)),
      pm25_hoy = as.numeric(pm25_hoy),
      pm25_lag1 = as.numeric(pm25_lag1),
      pm25_lag7 = as.numeric(pm25_lag7),
      ultima_conc = as.numeric(ultima_conc),
      antiguedad_dias = as.numeric(antiguedad_dias)
    )
}

# Imputacion y marcas de NA aprendidas SOLO en el entrenamiento del bloque.
preparar_bloque <- function(entrenamiento, validacion) {
  ent <- base_modelo(entrenamiento)
  val <- base_modelo(validacion)

  for (v in variables_numericas) {
    mediana_ent <- median(ent[[v]], na.rm = TRUE)
    if (!is.finite(mediana_ent)) mediana_ent <- 0

    nueva <- paste0(v, "_faltante")
    ent[[nueva]] <- as.integer(is.na(ent[[v]]))
    val[[nueva]] <- as.integer(is.na(val[[v]]))

    ent[[v]][is.na(ent[[v]])] <- mediana_ent
    val[[v]][is.na(val[[v]])] <- mediana_ent
  }

  if (anyNA(ent) || anyNA(val)) {
    stop("Quedaron NA despues de preparar el bloque. Revisar Station, fechas y respuesta.")
  }
  list(ent = ent, val = val)
}

# Hiperparametros: 7 ridge, 6 arbol, 6 bosque.
lambdas <- c(0.05, 0.2, 1, 3, 10, 30, 100)
cps <- c(0.0001, 0.0005, 0.001, 0.003, 0.01, 0.03)
grid_bosque <- expand.grid(mtry = c(3, 6, 10), hoja = c(10, 30))

predicciones_cv <- bind_rows(lapply(names(particiones), function(nombre) {
  cat("Ajustando", nombre, "\n")

  # 1. Reutilizar los mismos bloques que ya construyeron.
  ent_raw <- particiones[[nombre]]$entrenamiento %>%
    filter(!is.na(pm25_futuro))

  # Las mismas filas que usan para comparar ambas referencias.
  estacional_bloque <- predicciones_estacionales %>%
    filter(Bloque == nombre) %>%
    dplyr::select(Station, Date.Local, pred_estacional)

  val_raw <- particiones[[nombre]]$validacion %>%
    left_join(estacional_bloque, by = c("Station", "Date.Local")) %>%
    filter(!is.na(pm25_futuro), !is.na(ultima_conc),
           !is.na(pred_estacional))

  if (nrow(ent_raw) == 0 || nrow(val_raw) == 0) {
    stop(paste("Bloque sin datos validos:", nombre))
  }

  datos <- preparar_bloque(ent_raw, val_raw)
  ent <- datos$ent
  val <- datos$val

  # Guarda las predicciones fila a fila para comparar, diagnosticar y graficar.
  registrar <- function(modelo, configuracion, predicho) {
    stopifnot(length(predicho) == nrow(val_raw))
    tibble(
      Bloque = nombre,
      Modelo = modelo,
      Configuracion = configuracion,
      Station = as.character(val_raw$Station),
      fecha_objetivo = as.Date(val_raw$Date.Local) + 1,
      observado = val$pm25_futuro,
      predicho = as.numeric(predicho)
    )
  }

  pred <- list(
    ultima = registrar("Referencia ultima", "sin ajuste", val_raw$ultima_conc),
    estacional = registrar("Referencia estacional", "sin ajuste",
                           val_raw$pred_estacional)
  )

  # 2. M1: ridge (la estandarizacion se aprende en cada entrenamiento).
  formula_x <- ~ . - pm25_futuro - 1
  x_ent <- model.matrix(formula_x, data = ent)
  x_val <- model.matrix(formula_x, data = val)
  stopifnot(identical(colnames(x_ent), colnames(x_val)))

  ridge <- glmnet(
    x = x_ent, y = ent$pm25_futuro,
    family = "gaussian", alpha = 0,
    lambda = sort(lambdas, decreasing = TRUE),
    standardize = TRUE
  )

  for (i in seq_along(lambdas)) {
    lam <- lambdas[i]
    p <- predict(ridge, newx = x_val, s = lam)
    pred[[paste0("ridge_", i)]] <- registrar(
      "Ridge", paste0("lambda=", lam), p
    )
  }

  # 3. M2: hacer crecer un arbol y podarlo con varios valores de cp.
  arbol_grande <- rpart(
    pm25_futuro ~ ., data = ent, method = "anova",
    control = rpart.control(cp = 0, minsplit = 40, minbucket = 15,
                            maxdepth = 10, xval = 0)
  )

  for (i in seq_along(cps)) {
    cp <- cps[i]
    arbol <- prune(arbol_grande, cp = cp)
    p <- predict(arbol, newdata = val)
    pred[[paste0("arbol_", i)]] <- registrar(
      "Arbol podado", paste0("cp=", cp), p
    )
  }

  # 4. M3: bosque aleatorio.
  p_disponibles <- ncol(ent) - 1L
  for (i in seq_len(nrow(grid_bosque))) {
    mtry <- min(grid_bosque$mtry[i], p_disponibles)
    hoja <- grid_bosque$hoja[i]

    bosque <- ranger(
      pm25_futuro ~ ., data = ent,
      num.trees = 200,
      mtry = mtry,
      min.node.size = hoja,
      respect.unordered.factors = "order",
      seed = 3407,
      num.threads = 2
    )
    p <- predict(bosque, data = val)$predictions
    pred[[paste0("bosque_", i)]] <- registrar(
      "Bosque aleatorio", paste0("mtry=", mtry, ";hoja=", hoja), p
    )
  }

  bind_rows(pred)
}))

# 5. Metricas de cada configuracion, separadas por bloque.
resultados_cv <- predicciones_cv %>%
  group_by(Bloque, Modelo, Configuracion) %>%
  summarise(
    n = n(),
    MAE = mean(abs(observado - predicho)),
    MSE = mean((observado - predicho)^2),
    RMSE = sqrt(MSE),
    .groups = "drop"
  )

print(resultados_cv, n = Inf)

# 6. Resumen ponderado por numero de filas de validacion.
#    La seleccion usa MAE; RMSE es complementaria.
resumen_cv <- resultados_cv %>%
  group_by(Modelo, Configuracion) %>%
  summarise(
    bloques = n_distinct(Bloque),
    n_total = sum(n),
    MAE = sum(n * MAE) / sum(n),
    MSE = sum(n * MSE) / sum(n),
    RMSE = sqrt(MSE),
    .groups = "drop"
  ) %>%
  arrange(Modelo, MAE)

print(resumen_cv, n = Inf)

# 7. Mejor configuracion de cada familia (y ambas referencias).
mejores <- resumen_cv %>%
  group_by(Modelo) %>%
  slice_min(MAE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(MAE)

print(mejores %>% dplyr::select(Modelo, Configuracion, n_total, MAE, RMSE))

# 8. Tabla para comprobar estabilidad entre bloques.
detalle_mejores <- resultados_cv %>%
  inner_join(mejores %>% dplyr::select(Modelo, Configuracion),
             by = c("Modelo", "Configuracion")) %>%
  arrange(Modelo, Bloque)

print(detalle_mejores %>%
        dplyr::select(Bloque, Modelo, Configuracion, n, MAE, RMSE), n = Inf)

# 9. Predicciones de las configuraciones elegidas: estacion y estacion del ano.
predicciones_mejores <- predicciones_cv %>%
  inner_join(mejores %>% dplyr::select(Modelo, Configuracion),
             by = c("Modelo", "Configuracion")) %>%
  mutate(
    temporada = case_when(
      month(fecha_objetivo) %in% c(12, 1, 2) ~ "Invierno boreal",
      month(fecha_objetivo) %in% c(3, 4, 5) ~ "Primavera boreal",
      month(fecha_objetivo) %in% c(6, 7, 8) ~ "Verano boreal",
      TRUE ~ "Otono boreal"
    )
  )

errores_por_estacion <- predicciones_mejores %>%
  group_by(Modelo, Station) %>%
  summarise(n = n(),
            MAE = mean(abs(observado - predicho)),
            RMSE = sqrt(mean((observado - predicho)^2)),
            .groups = "drop")

errores_por_temporada <- predicciones_mejores %>%
  group_by(Modelo, temporada) %>%
  summarise(n = n(),
            MAE = mean(abs(observado - predicho)),
            RMSE = sqrt(mean((observado - predicho)^2)),
            .groups = "drop")

print(errores_por_estacion, n = Inf)
print(errores_por_temporada, n = Inf)

# Proxima etapa: reajuste con todas las respuestas disponibles en 2019-2022,
# y evaluacion UNA SOLA VEZ en prueba 2023-2024. NO se hace en este script.
