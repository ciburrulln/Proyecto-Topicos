# ================================================================
# CASO 02 - PM2.5: validacion temporal SIN IMPUTACION
# Se ejecuta DESPUES de crear pm25_diario, particiones y
# predicciones_estacionales con el codigo del equipo.
# Entrena con 2019-2021 segun bloque; valida 2020, 2021, 2022.
# NO consulta las respuestas de la reserva (2023-2024).
# Mantiene intactos todos los NA de pm25_diario.
# ================================================================

paquetes <- c("dplyr", "lubridate", "glmnet", "rpart", "ranger")
for (paq in paquetes) {
  if (!requireNamespace(paq, quietly = TRUE)) install.packages(paq)
}
library(dplyr)
library(lubridate)
library(glmnet)
library(rpart)
library(ranger)

stopifnot(exists("pm25_diario"),
          exists("particiones"),
          exists("predicciones_estacionales"))

set.seed(3407)

# NUNCA cambiar pm25_hoy, pm25_lag1, pm25_lag7 ni pm25_futuro.
# ultima_conc sigue siendo la referencia historica que ya calcularon;
# no se usa para rellenar ninguna de esas cuatro columnas.

# Niveles de estaciones conocidos en DESARROLLO (sin mirar prueba).
niveles_estacion <- pm25_diario %>%
  filter(as.Date(Date.Local) >= as.Date("2019-01-01"),
         as.Date(Date.Local) < as.Date("2023-01-01")) %>%
  pull(Station) %>% as.character() %>% unique() %>% sort()

# Predictores elegidos previamente; el objeto original no se altera.
# Nota: en casos completos pm25_hoy == ultima_conc y antiguedad_dias == 0;
# pueden quitar estas dos ultimas variables en un analisis de sensibilidad.
base_modelo <- function(datos) {
  datos %>%
    transmute(
      pm25_futuro = as.numeric(pm25_futuro),
      Station = factor(as.character(Station), levels = niveles_estacion),
      mes_futuro = factor(as.character(mes_futuro),
                          levels = as.character(1:12)),
      pm25_hoy = as.numeric(pm25_hoy),
      pm25_lag1 = as.numeric(pm25_lag1),
      pm25_lag7 = as.numeric(pm25_lag7),
      ultima_conc = as.numeric(ultima_conc),
      antiguedad_dias = as.numeric(antiguedad_dias)
    )
}

# Filtrar SOLO para ajustar/predecir: NINGUN valor se rellena.
# val_ok sirve para conservar la alineacion con los datos originales.
preparar_bloque <- function(entrenamiento, validacion) {
  ent_base <- base_modelo(entrenamiento)
  val_base <- base_modelo(validacion)

  ent_ok <- complete.cases(ent_base)
  val_ok <- complete.cases(val_base)

  list(
    ent = ent_base[ent_ok, , drop = FALSE],
    val = val_base[val_ok, , drop = FALSE],
    ent_ok = ent_ok,
    val_ok = val_ok
  )
}

# 7 Ridge, 6 arbol, 6 bosque (19 configuraciones en total).
lambdas <- c(0.05, 0.2, 1, 3, 10, 30, 100)
cps <- c(0.0001, 0.0005, 0.001, 0.003, 0.01, 0.03)
grid_bosque <- expand.grid(mtry = c(2, 4, 7), hoja = c(10, 30))

resultados_bloques <- lapply(names(particiones), function(nombre) {
  message("Ajustando ", nombre)

  # Entrenamiento: respuestas observadas y anteriores a la validacion.
  ent_raw <- particiones[[nombre]]$entrenamiento %>%
    filter(!is.na(pm25_futuro))

  # Referencia estacional ya calculada por ustedes usando SOLO el
  # entrenamiento de este mismo bloque.
  estacional_bloque <- predicciones_estacionales %>%
    filter(Bloque == nombre) %>%
    select(Station, Date.Local, pred_estacional)

  # Evitar uniones que dupliquen estaciones-dias.
  if (anyDuplicated(estacional_bloque[c("Station", "Date.Local")])) {
    stop("Fechas duplicadas en referencia estacional del ", nombre)
  }

  val_raw <- particiones[[nombre]]$validacion %>%
    left_join(estacional_bloque, by = c("Station", "Date.Local"))

  # Comprobacion temporal: ninguna respuesta de entrenamiento
  # puede corresponder al periodo validado.
  if (nrow(ent_raw) == 0 || nrow(val_raw) == 0) {
    stop("Bloque vacio: ", nombre)
  }
  if (max(as.Date(ent_raw$Date.Local) + 1) >=
      min(as.Date(val_raw$Date.Local) + 1)) {
    stop("Posible fuga por fechas en ", nombre)
  }

  # Sin imputacion: identificar las filas completas en train y val.
  datos <- preparar_bloque(ent_raw, val_raw)

  # La comparacion principal requiere que ambas referencias tambien
  # esten disponibles en las mismas filas de validacion.
  val_comun_ok <- datos$val_ok &
    !is.na(val_raw$ultima_conc) &
    !is.na(val_raw$pred_estacional)

  ent <- datos$ent
  val <- base_modelo(val_raw[val_comun_ok, , drop = FALSE])
  val_raw_comun <- val_raw[val_comun_ok, , drop = FALSE]

  auditoria <- tibble(
    Bloque = nombre,
    n_validacion_total = nrow(val_raw),
    n_respuesta_observada = sum(!is.na(val_raw$pm25_futuro)),
    n_modelo_casos_completos = sum(datos$val_ok),
    n_comparacion_comun = sum(val_comun_ok),
    n_descartadas_por_NA_modelo = sum(!is.na(val_raw$pm25_futuro)) -
      sum(datos$val_ok),
    n_completas_sin_ambas_referencias =
      sum(datos$val_ok) - sum(val_comun_ok)
  )

  if (nrow(ent) < 50 || nrow(val) == 0) {
    stop("Insuficientes filas completas para modelar en ", nombre,
         ". Revisa la tabla de NA y la lista de predictores.")
  }

  # Guardar predicciones sobre exactamente las mismas estaciones-fechas.
  registrar <- function(modelo, configuracion, predicho) {
    predicho <- as.numeric(predicho)
    if (length(predicho) != nrow(val) || anyNA(predicho)) {
      stop("Predicciones invalidas de ", modelo, " en ", nombre)
    }
    tibble(
      Bloque = nombre,
      Modelo = modelo,
      Configuracion = configuracion,
      Station = as.character(val_raw_comun$Station),
      fecha_objetivo = as.Date(val_raw_comun$Date.Local) + 1,
      observado = val$pm25_futuro,
      predicho = predicho
    )
  }

  pred <- list(
    ultima = registrar("Referencia ultima", "sin ajuste",
                      val_raw_comun$ultima_conc),
    estacional = registrar("Referencia estacional", "sin ajuste",
                          val_raw_comun$pred_estacional)
  )

  # M1: Ridge. Sin NAs (por exclusion de filas), escala interna
  # aprendida solo del entrenamiento de este bloque.
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
    pred[[paste0("ridge_", i)]] <- registrar(
      "Ridge", paste0("lambda=", lam),
      predict(ridge, newx = x_val, s = lam)
    )
  }

  # M2: arbol de regresion podado.
  arbol_grande <- rpart(
    pm25_futuro ~ ., data = ent, method = "anova",
    control = rpart.control(
      cp = 0, minsplit = 40, minbucket = 15,
      maxdepth = 10, xval = 0
    )
  )
  for (i in seq_along(cps)) {
    cp <- cps[i]
    arbol <- prune(arbol_grande, cp = cp)
    pred[[paste0("arbol_", i)]] <- registrar(
      "Arbol podado", paste0("cp=", cp),
      predict(arbol, newdata = val)
    )
  }

  # M3: bosque aleatorio; no se imputan NA dentro ni fuera.
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
      seed = 3407 + i,
      num.threads = 2
    )
    pred[[paste0("bosque_", i)]] <- registrar(
      "Bosque aleatorio", paste0("mtry=", mtry, ";hoja=", hoja),
      predict(bosque, data = val)$predictions
    )
  }

  list(predicciones = bind_rows(pred), auditoria = auditoria)
})

predicciones_cv <- bind_rows(lapply(resultados_bloques, `[[`, "predicciones"))
auditoria_filas <- bind_rows(lapply(resultados_bloques, `[[`, "auditoria"))

# Cuantas filas se pierden por NA sin imputacion.
print(auditoria_filas, n = Inf)

# MAE y RMSE por bloque y configuracion.
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

# Agregar las predicciones retenidas de todos los bloques.
# La seleccion utiliza MAE; RMSE se informa como complemento.
resumen_cv <- resultados_cv %>%
  group_by(Modelo, Configuracion) %>%
  summarise(
    bloques = n_distinct(Bloque),
    n_total = sum(n),
    MAE = weighted.mean(MAE, n),
    MSE = weighted.mean(MSE, n),
    RMSE = sqrt(MSE),
    .groups = "drop"
  ) %>%
  arrange(Modelo, MAE)

mejores <- resumen_cv %>%
  group_by(Modelo) %>%
  slice_min(MAE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(MAE)

print(mejores %>% select(Modelo, Configuracion, n_total, MAE, RMSE),
      n = Inf)

# Ganancia contra la ultima concentracion en la MISMA muestra.
mae_referencia <- mejores %>%
  filter(Modelo == "Referencia ultima") %>%
  pull(MAE)

comparacion_con_referencia <- mejores %>%
  mutate(
    diferencia_MAE = MAE - mae_referencia,
    mejora_pct_MAE = 100 * (mae_referencia - MAE) / mae_referencia
  ) %>%
  select(Modelo, Configuracion, n_total, MAE, RMSE,
         diferencia_MAE, mejora_pct_MAE)
print(comparacion_con_referencia, n = Inf)

# Estabilidad en cada bloque, para las configuraciones seleccionadas.
detalle_mejores <- resultados_cv %>%
  inner_join(mejores %>% select(Modelo, Configuracion),
             by = c("Modelo", "Configuracion")) %>%
  arrange(Modelo, Bloque)
print(detalle_mejores %>%
        select(Bloque, Modelo, Configuracion, n, MAE, RMSE), n = Inf)

# Errores por estacion y temporada (las mismas filas comunes).
predicciones_mejores <- predicciones_cv %>%
  inner_join(mejores %>% select(Modelo, Configuracion),
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

# La reserva 2023-2024 permanece sin evaluar. Seleccionar y congelar
# un procedimiento con desarrollo antes del ajuste final y prueba.
