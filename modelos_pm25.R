
library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)
library(rpart)
library(ranger)

SEMILLA <- 2221268
set.seed(2221268)

# 1. BASE ANALITICA: SOLO OBSERVACIONES COMPLETAS
# Los tres modelos usan los mismos predictores y las mismas filas.
pm25_diario <- pm25_diario %>% mutate(Date.Local = as.Date(Date.Local))
niveles_estacion <- pm25_diario %>% pull(Station) %>% as.character() %>% unique() %>% sort()
niveles_estacion_ano <- pm25_diario %>% pull(estacion_ano) %>% as.character() %>% unique() %>% sort()

# No está ultima_conc ni antiguedad_dias dado que usamos solo 
# las observaciones completas

predictores <- c("Station", "pm25_hoy", "pm25_lag1", "ano", "mes", "dia_semana", "estacion_ano")
variables_completas <- c("pm25_futuro", predictores)

datos_preparados <- pm25_diario %>%
  mutate(fecha_objetivo = Date.Local + days(1), Station = factor(as.character(Station), levels = niveles_estacion),
    # Año numérico del día que queremos predecir
    ano = year(fecha_objetivo),
    # Mes del día que queremos predecir
    mes = factor(month(fecha_objetivo), levels = 1:12),
    # Día de la semana del día objetivo
    dia_semana = factor(wday(fecha_objetivo, week_start = 1), levels = 1:7, labels = c(
      "Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")),
    # Estación del año en el hemisferio norte
    estacion_ano = case_when(month(fecha_objetivo) %in% 3:5 ~ "Primavera",
      month(fecha_objetivo) %in% 6:8 ~ "Verano",
      month(fecha_objetivo) %in% 9:11 ~ "Otoño",
      TRUE ~ "Invierno"),
    estacion_ano = factor(estacion_ano, levels = c("Invierno", "Primavera", "Verano", "Otoño")),
    estacion_objetivo = estacion_ano) %>%
  filter(fecha_objetivo >= as.Date("2019-01-01"), fecha_objetivo <= as.Date("2024-12-31") )

# Mostrar cuantas filas se pierden en DESARROLLO, sin mirar las respuestas de prueba.
cobertura_desarrollo <- datos_preparados %>%
  filter(fecha_objetivo <= as.Date("2022-12-31")) %>%
  mutate(completa = complete.cases(pick(all_of(variables_completas)))) %>%
  group_by(anio = year(fecha_objetivo)) %>%
  summarise(total_filas = n(), filas_completas = sum(completa),
            filas_excluidas = sum(!completa), .groups = "drop")
print(cobertura_desarrollo)

# SIN IMPUTACION. El mismo filtro se aplica antes de formar las particiones.
datos_modelo <- datos_preparados %>%
  filter(complete.cases(pick(all_of(variables_completas))))

# Formula comun: corresponde a la lista de predictores seleccionada.
formula_modelo <- pm25_futuro ~ Station + pm25_hoy + pm25_lag1 + mes + ano + dia_semana + estacion_ano

matriz_x <- function(datos, formula = formula_modelo) {
  model.matrix(formula, data = datos)[, -1, drop = FALSE]}

metricas <- function(y, pred) {
  tibble(n = length(y), MAE = mean(abs(y - pred)), RMSE = sqrt(mean((y - pred)^2)))}

# 2. LOS TRES BLOQUES TEMPORALES EXISTENTES 
# La fecha de la respuesta (t+1) determina la particion.
particiones_modelo <- lapply(bloques, function(b) {
  ent <- datos_modelo %>% filter(fecha_objetivo >= b$inicio_train, fecha_objetivo < b$inicio_val)
  val <- datos_modelo %>% filter(fecha_objetivo >= b$inicio_val, fecha_objetivo <= b$fin_val)
  if (nrow(ent) == 0 || nrow(val) == 0) {
    stop("Particion sin observaciones: revisar fechas y cobertura.")}
  if (max(ent$fecha_objetivo) >= min(val$fecha_objetivo)) {
    stop("La respuesta de entrenamiento invade la validacion.")}
  list(entrenamiento = ent, validacion = val)})

control_particiones <- bind_rows(lapply(names(particiones_modelo), function(nombre) {
  p <- particiones_modelo[[nombre]]
  tibble(Bloque = nombre, n_train = nrow(p$entrenamiento),
         n_validacion = nrow(p$validacion),
         inicio_entrenamiento = min(p$ent$fecha_objetivo),
         fin_entrenamiento = max(p$ent$fecha_objetivo),
         inicio_validacion = min(p$validacion$fecha_objetivo),
         fin_validacion = max(p$validacion$fecha_objetivo))
}))
print(control_particiones)

# 3. M1: RIDGE Y LASSO (20 CONFIGURACIONES EN TOTAL)
# Las columnas se estandarizan SOLO con entrenamiento (glmnet).
# Para cada alpha se ajusta una ruta de 10 valores de lambda.
lambdas <- exp(seq(log(20), log(0.02), length.out = 10))

cv_m1 <- bind_rows(lapply(names(particiones_modelo), function(nombre) {
  ent <- particiones_modelo[[nombre]]$entrenamiento
  val <- particiones_modelo[[nombre]]$validacion
  x_ent <- matriz_x(ent)
  x_val <- matriz_x(val)
  
  bind_rows(lapply(c(0, 1), function(a) {
    ajuste <- glmnet(x_ent, ent$pm25_futuro, alpha = a, lambda = lambdas, standardize = TRUE)
    pronosticos <- as.matrix(predict(ajuste, newx = x_val, s = lambdas))
    bind_rows(lapply(seq_along(lambdas), function(j) {
      metricas(val$pm25_futuro, as.numeric(pronosticos[, j])) %>%
        mutate(Bloque = nombre, alpha = a, lambda = lambdas[j],
               Modelo = if_else(a == 0, "Ridge", "Lasso"))
    }))
  }))
}))

resumen_m1 <- cv_m1 %>%
  group_by(Modelo, alpha, lambda) %>%
  summarise(MAE_medio = mean(MAE), RMSE_medio = mean(RMSE), .groups = "drop") %>%
  arrange(MAE_medio, RMSE_medio)
mejor_m1 <- slice_head(resumen_m1, n = 1)
print(mejor_m1)

# 4. M2: ARBOL DE REGRESION CON PODA COSTO-COMPLEJIDAD
# Se construye un arbol grande y se prueban 8 valores de cp.
cp_grid <- c(0.00005, 0.0002, 0.0005, 0.001,
             0.003, 0.01, 0.02, 0.05)

construir_arbol <- function(ent) {
  rpart(formula_modelo, data = ent, method = "anova",
        control = rpart.control(cp = min(cp_grid), minsplit = 40,
                                minbucket = 20, maxdepth = 15, xval = 0))
}

cv_arbol <- bind_rows(lapply(names(particiones_modelo), function(nombre) {
  ent <- particiones_modelo[[nombre]]$entrenamiento
  val <- particiones_modelo[[nombre]]$validacion
  arbol_grande <- construir_arbol(ent)
  
  bind_rows(lapply(cp_grid, function(cp_val) {
    arbol_podado <- prune(arbol_grande, cp = cp_val)
    pronostico <- as.numeric(predict(arbol_podado, newdata = val))
    metricas(val$pm25_futuro, pronostico) %>%
      mutate(Bloque = nombre, cp = cp_val)
  }))
}))

resumen_arbol <- cv_arbol %>%
  group_by(cp) %>%
  summarise(MAE_medio = mean(MAE), RMSE_medio = mean(RMSE), .groups = "drop") %>%
  arrange(MAE_medio, RMSE_medio)
mejor_arbol <- slice_head(resumen_arbol, n = 1)
print(mejor_arbol)

# 5. M3: BOSQUE ALEATORIO (6 CONFIGURACIONES)
# 150 arboles por ajuste; mtry y tamano minimo de nodo se validan.
grid_bosque <- expand.grid(mtry = c(2, 4, 6), min.node.size = c(10, 30))

construir_bosque <- function(ent, mtry, min_nodo, semilla = SEMILLA) {
  ranger(formula_modelo, data = ent, num.trees = 150,
         mtry = mtry, min.node.size = min_nodo,
         seed = semilla, num.threads = 2,
         respect.unordered.factors = "order")
}

cv_bosque <- bind_rows(lapply(seq_along(particiones_modelo), function(i) {
  nombre <- names(particiones_modelo)[i]
  ent <- particiones_modelo[[i]]$entrenamiento
  val <- particiones_modelo[[i]]$validacion
  
  bind_rows(lapply(seq_len(nrow(grid_bosque)), function(j) {
    config <- grid_bosque[j, ]
    ajuste <- construir_bosque(ent, config$mtry, config$min.node.size,
                               semilla = SEMILLA + 100 * i + j)
    pronostico <- predict(ajuste, data = val)$predictions
    metricas(val$pm25_futuro, pronostico) %>%
      mutate(Bloque = nombre, mtry = config$mtry,
             min.node.size = config$min.node.size)
  }))
}))

resumen_bosque <- cv_bosque %>%
  group_by(mtry, min.node.size) %>%
  summarise(MAE_medio = mean(MAE), RMSE_medio = mean(RMSE), .groups = "drop") %>%
  arrange(MAE_medio, RMSE_medio)
mejor_bosque <- slice_head(resumen_bosque, n = 1)
print(mejor_bosque)

# 6. REAJUSTAR LOS PARAMETROS ELEGIDOS EN CADA BLOQUE
# Genera una prediccion por observacion para comparacion pareada.
predecir_todos <- function(ent, val, semilla = SEMILLA) {
  ajuste_m1 <- glmnet(matriz_x(ent), ent$pm25_futuro,
                      alpha = mejor_m1$alpha, lambda = lambdas, standardize = TRUE)
  pred_m1 <- as.numeric(predict(ajuste_m1, newx = matriz_x(val), s = mejor_m1$lambda))
  arbol_grande <- construir_arbol(ent)
  arbol_podado <- prune(arbol_grande, cp = mejor_arbol$cp)
  pred_arbol <- as.numeric(predict(arbol_podado, newdata = val))
  bosque <- construir_bosque(ent, mejor_bosque$mtry, mejor_bosque$min.node.size, semilla = semilla)
  pred_bosque <- as.numeric(predict(bosque, data = val)$predictions)
  
  tibble(Station = val$Station, fecha_objetivo = val$fecha_objetivo,
         estacion_objetivo = val$estacion_objetivo, pm25_futuro = val$pm25_futuro,
         pred_persistencia = val$ultima_conc, pred_M1 = pred_m1, pred_arbol = pred_arbol, pred_bosque = pred_bosque)}

# Conservar las mismas semillas usadas al evaluar la configuracion RF elegida.
indice_mejor_bosque <- which(
  grid_bosque$mtry == mejor_bosque$mtry &
    grid_bosque$min.node.size == mejor_bosque$min.node.size)[1]

pred_validacion <- bind_rows(lapply(seq_along(particiones_modelo), function(i) {
  p <- particiones_modelo[[i]]
  predecir_todos(p$entrenamiento, p$validacion,
                 semilla = SEMILLA + 100 * i + indice_mejor_bosque) %>%
    mutate(Bloque = names(particiones_modelo)[i])}))

# 7. METRICAS COMPARABLES: MISMAS FILAS PARA LOS CUATRO METODOS
nombres_pred <- c("Referencia: persistencia" = "pred_persistencia",
                  "M1: regresion penalizada" = "pred_M1",
                  "M2: arbol podado" = "pred_arbol",
                  "M3: bosque aleatorio" = "pred_bosque")

comparar_modelos <- function(datos) {
  bind_rows(lapply(names(nombres_pred), function(nombre) {
    variable <- nombres_pred[[nombre]]
    metricas(datos$pm25_futuro, datos[[variable]]) %>%
      mutate(Modelo = nombre)})) %>%
    dplyr::select(Modelo, n, MAE, RMSE) %>%
    mutate(diferencia_MAE_persistencia = MAE - MAE[Modelo == "Referencia: persistencia"])
}

comparacion_validacion <- comparar_modelos(pred_validacion) %>% arrange(MAE)
print(comparacion_validacion)

# Comparar tambien cada ventana por separado.
# La dispersion de ventanas solapadas NO equivale a un IC independiente.
comparacion_bloques <- bind_rows(lapply(names(particiones_modelo), function(nombre) {
  comparar_modelos(filter(pred_validacion, Bloque == nombre)) %>%
    mutate(Bloque = nombre)}))
print(comparacion_bloques)

tabla_bloques <- comparacion_bloques %>%
  mutate(
    Año = case_when(
      Bloque == "bloque_1" ~ "Bloque 1",
      Bloque == "bloque_2" ~ "Bloque 2",
      Bloque == "bloque_3" ~ "Bloque 3"
    ),
    Modelo = recode(
      Modelo,
      "Referencia: persistencia" = "Persistencia",
      "M1: regresion penalizada" = "M1 Lasso",
      "M2: arbol podado" = "M2 Arbol",
      "M3: bosque aleatorio" = "M3 Bosque"
    )
  ) %>%
  dplyr::select(Año, Modelo, MAE) %>%
  tidyr::pivot_wider(
    names_from = Modelo,
    values_from = MAE
  ) %>%
  mutate(across(where(is.numeric) & !matches("Año"), ~ round(.x, 2)))

print(tabla_bloques)

# Seleccion principal: promedio no ponderado del MAE de los tres bloques.
# Coherente con la regla usada para escoger hiperparametros.
criterio_seleccion <- comparacion_bloques %>%
  group_by(Modelo) %>%
  summarise(MAE_medio_bloques = mean(MAE),
            RMSE_medio_bloques = mean(RMSE), .groups = "drop") %>%
  arrange(MAE_medio_bloques, RMSE_medio_bloques)
procedimiento_elegido <- slice_head(criterio_seleccion, n = 1)
print(criterio_seleccion)
print(procedimiento_elegido)

#### TABLA FINAL DE COMPARACION ####

# MAE promedio de la referencia
mae_ref <- criterio_seleccion %>%
  filter(Modelo == "Referencia: persistencia") %>%
  pull(MAE_medio_bloques)

# Comparar todos los procedimientos
tabla_comparativa <- criterio_seleccion %>%
  mutate(
    diferencia_MAE = MAE_medio_bloques - mae_ref,
    
    mejora_porcentual = 100 *
      (mae_ref - MAE_medio_bloques) / mae_ref
  ) %>%
  arrange(MAE_medio_bloques)

print(tabla_comparativa)

# 8. DIAGNOSTICO POR ESTACION, ESTACION DEL ANO Y EPISODIOS ALTOS
# Umbral obtenido UNICAMENTE con respuestas de desarrollo (2019-2022).
umbral_alto <- datos_modelo %>%
  filter(fecha_objetivo <= as.Date("2022-12-31")) %>%
  pull(pm25_futuro) %>%
  quantile(probs = 0.90, names = FALSE)

diagnostico_largo <- pred_validacion %>%
  mutate(episodio = if_else(pm25_futuro >= umbral_alto, "Concentracion alta", "Resto")) %>%
  pivot_longer(cols = all_of(unname(nombres_pred)), names_to = "Modelo", values_to = "Prediccion") %>%
  mutate(error_abs = abs(pm25_futuro - Prediccion),
         error_cuadrado = (pm25_futuro - Prediccion)^2)

errores_estacion <- diagnostico_largo %>%
  group_by(Station, Modelo) %>%
  summarise(n = n(), MAE = mean(error_abs),
            RMSE = sqrt(mean(error_cuadrado)), .groups = "drop")

errores_temporada <- diagnostico_largo %>%
  group_by(estacion_objetivo, Modelo) %>%
  summarise(n = n(), MAE = mean(error_abs),
            RMSE = sqrt(mean(error_cuadrado)), .groups = "drop")

errores_episodios <- diagnostico_largo %>%
  group_by(episodio, Modelo) %>%
  summarise(n = n(), MAE = mean(error_abs),
            RMSE = sqrt(mean(error_cuadrado)), .groups = "drop")

print(errores_estacion)
print(errores_temporada)
print(errores_episodios)

# Figura: error por temporada, con los cuatro metodos y sus denominadores.
library(ggplot2)
figura_temporada <- errores_temporada %>%
  mutate(
    Modelo = recode(
      Modelo,
      "pred_M1" = "Lasso",
      "pred_arbol" = "Árbol podado",
      "pred_bosque" = "Bosque aleatorio",
      "pred_persistencia" = "Persistencia"
    )
  ) %>%
  ggplot(aes(
    x = estacion_objetivo,
    y = MAE,
    color = Modelo,
    group = Modelo
  )) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  labs(
    x = "Estación del año",
    y = "MAE (µg/m³)",
    color = "Procedimiento"
  ) +
  theme_minimal()

print(figura_temporada)

comparacion_estaciones <- errores_estacion %>%
  select(Station, Modelo, MAE) %>%
  pivot_wider(
    names_from = Modelo,
    values_from = MAE
  ) %>%
  mutate(
    diferencia = pred_M1 - pred_persistencia
  )

comparacion_estaciones %>%
  summarise(
    estaciones_total = n(),
    mejora_lasso = sum(diferencia < 0),
    mejora_persistencia = sum(diferencia > 0),
    empates = sum(diferencia == 0)
  )

# 9. DOS ANÁLISIS DE SENSIBILIDAD EN DESARROLLO
# Comprobar que ninguna partición incluya la prueba final.
stopifnot(all(vapply(particiones_modelo, function(p) {
    all(p$entrenamiento$fecha_objetivo < as.Date("2023-01-01")) &&
      all(p$validacion$fecha_objetivo < as.Date("2023-01-01"))}, logical(1))))

# A. REPRESENTACIÓN: M1 CON pm25_lag1 VS SIN pm25_lag1
# Mismas observaciones, bloques y predictores restantes.
formula_sin_ano <- update(formula_modelo, . ~ . - pm25_lag1)
# Evaluar las mismas 20 configuraciones de M1, ahora sin año.
cv_m1_sin_ano <- bind_rows(
  lapply(names(particiones_modelo), function(nombre) {
    ent <- particiones_modelo[[nombre]]$entrenamiento
    val <- particiones_modelo[[nombre]]$validacion
    x_ent <- matriz_x(ent, formula_sin_ano)
    x_val <- matriz_x(val, formula_sin_ano)
    bind_rows(lapply(c(0, 1), function(a) {
      ajuste <- glmnet(x = x_ent, y = ent$pm25_futuro, alpha = a, lambda = lambdas, standardize = TRUE)
      pronosticos <- as.matrix(predict(ajuste, newx = x_val, s = lambdas))
      bind_rows(lapply(seq_along(lambdas), function(j) {
        metricas(val$pm25_futuro, as.numeric(pronosticos[, j])) %>%
          mutate(Bloque = nombre, alpha = a, lambda = lambdas[j], Modelo = if_else(a == 0, "Ridge", "Lasso"))}))
    }))
  }))
# Seleccionar hiperparámetros con el mismo criterio original.
resumen_m1_sin_ano <- cv_m1_sin_ano %>%
  group_by(Modelo, alpha, lambda) %>%
  summarise(MAE_medio = mean(MAE), RMSE_medio = mean(RMSE),
    .groups = "drop"
  ) %>%
  arrange(MAE_medio, RMSE_medio)

mejor_m1_sin_ano <- slice_head(resumen_m1_sin_ano, n = 1)

# Mostrar qué configuración se seleccionó en cada versión.
configuraciones_sensibilidad <- bind_rows(
  mejor_m1 %>% mutate(Representacion = "Con año"),
  mejor_m1_sin_ano %>% mutate(Representacion = "Sin año")
) %>%
  dplyr::select(Representacion, everything())

print(configuraciones_sensibilidad)

# Recuperar las métricas de las configuraciones elegidas.
# cv_m1 ya fue calculado en la sección 3.
resultados_con_ano <- cv_m1 %>%
  filter(alpha == mejor_m1$alpha, lambda == mejor_m1$lambda) %>%
  transmute(Bloque, n, MAE_con_ano = MAE, RMSE_con_ano = RMSE)

resultados_sin_ano <- cv_m1_sin_ano %>%
  filter(alpha == mejor_m1_sin_ano$alpha, lambda == mejor_m1_sin_ano$lambda) %>%
  transmute(Bloque, n, MAE_sin_ano = MAE, RMSE_sin_ano = RMSE)

# Comparación por bloque.
sensibilidad_ano <- resultados_con_ano %>%
  inner_join(resultados_sin_ano, by = c("Bloque", "n")) %>%
  mutate(diferencia_MAE = MAE_sin_ano - MAE_con_ano, diferencia_RMSE = RMSE_sin_ano - RMSE_con_ano)

print(sensibilidad_ano)

# Resumen: igual peso para cada bloque.
resumen_sensibilidad_ano <- sensibilidad_ano %>%
  summarise(MAE_con_ano = mean(MAE_con_ano), MAE_sin_ano = mean(MAE_sin_ano),
    RMSE_con_ano = mean(RMSE_con_ano), RMSE_sin_ano = mean(RMSE_sin_ano)) %>%
  mutate(diferencia_MAE = MAE_sin_ano - MAE_con_ano, diferencia_RMSE = RMSE_sin_ano - RMSE_con_ano)

print(resumen_sensibilidad_ano)

# B. POBLACIÓN: TODOS LOS DÍAS VS SIN EPISODIOS ALTOS
# Se mantienen las predicciones de los modelos originales.
# No se reajustan ni se vuelven a seleccionar hiperparámetros.
# umbral_alto es el percentil 90 de desarrollo, ya calculado
# en la sección 8. No es un umbral sanitario.
# Documentar las observaciones excluidas por bloque.
cobertura_sensibilidad <- pred_validacion %>% group_by(Bloque) %>%
  summarise(n_total = n(), n_sin_altos = sum(pm25_futuro < umbral_alto), 
            n_altos = sum(pm25_futuro >= umbral_alto), porcentaje_excluido = 100 * n_altos / n_total, .groups = "drop")

print(umbral_alto)
print(cobertura_sensibilidad)

if (any(cobertura_sensibilidad$n_sin_altos == 0)) {
  stop("Hay un bloque sin observaciones por debajo del umbral.")
}

# Comparación dentro de cada bloque y escenario.
sensibilidad_poblacion_bloques <- bind_rows(
  lapply(names(particiones_modelo), function(nombre) {
    datos_bloque <- pred_validacion %>% filter(Bloque == nombre)
    datos_sin_altos <- datos_bloque %>% filter(pm25_futuro < umbral_alto)
    bind_rows(comparar_modelos(datos_bloque) %>% mutate(Escenario = "Todos los días"),
              comparar_modelos(datos_sin_altos) %>% mutate(Escenario = "Sin episodios altos")) %>%
      mutate(Bloque = nombre)}))
print(sensibilidad_poblacion_bloques)

# Resumir con igual peso para cada bloque, igual que en
# el criterio principal de selección de modelos.
sensibilidad_poblacion <- sensibilidad_poblacion_bloques %>%
  group_by(Escenario, Modelo) %>% summarise(n_evaluaciones = sum(n),
    MAE_medio_bloques = mean(MAE), RMSE_medio_bloques = mean(RMSE),
    diferencia_MAE_persistencia = mean(diferencia_MAE_persistencia), .groups = "drop") %>%
  group_by(Escenario) %>% mutate(Posicion_MAE = min_rank(MAE_medio_bloques)) %>%
  ungroup() %>%
  dplyr::select(Escenario, Modelo, Posicion_MAE, everything()) %>%
  arrange(Escenario, Posicion_MAE)
print(sensibilidad_poblacion)

# 10. PRUEBA FINAL SELLADA (2023-2024)

# Cambiar a TRUE solo cuando todas las decisiones
# de modelamiento esten cerradas.
EVALUAR_PRUEBA_FINAL <- TRUE

if (EVALUAR_PRUEBA_FINAL) {
  
  #### ENTRENAMIENTO Y PRUEBA FINAL ####
  
  entrenamiento_final <- datos_modelo %>%
    filter(
      fecha_objetivo >= as.Date("2019-01-01"),
      fecha_objetivo <= as.Date("2022-12-31")
    )
  
  prueba_final <- datos_modelo %>%
    filter(
      fecha_objetivo >= as.Date("2023-01-01"),
      fecha_objetivo <= as.Date("2024-12-31")
    )
  
  if (nrow(prueba_final) == 0) {
    stop("No hay casos completos en prueba final.")
  }
  
  # Reajustar los modelos con sus hiperparametros
  # previamente seleccionados en desarrollo.
  
  pred_prueba <- predecir_todos(
    entrenamiento_final,
    prueba_final,
    semilla = SEMILLA
  )
  
  # Verificar predicciones completas.
  stopifnot(all(complete.cases(
    pred_prueba[, c("pm25_futuro", unname(nombres_pred))]
  )))
  
  
  #### COMPARACION GENERAL EN PRUEBA ####
  
  comparacion_prueba <- comparar_modelos(pred_prueba) %>%
    arrange(MAE)
  
  print(comparacion_prueba)
  
  
  #### DIAGNOSTICO DE ERRORES ####
  
  # Todas las predicciones corresponden a prueba.
  # El umbral fue definido previamente en desarrollo.
  
  diagnostico_prueba <- pred_prueba %>%
    mutate(
      episodio = if_else(
        pm25_futuro >= umbral_alto,
        "Concentracion alta",
        "Resto"
      )
    ) %>%
    pivot_longer(
      cols = all_of(unname(nombres_pred)),
      names_to = "Modelo",
      values_to = "Prediccion"
    ) %>%
    mutate(
      error_abs = abs(pm25_futuro - Prediccion),
      error_cuadrado = (pm25_futuro - Prediccion)^2
    )
  
  
  #### A. ERRORES POR ESTACION ####
  
  prueba_por_estacion <- diagnostico_prueba %>%
    group_by(Station, Modelo) %>%
    summarise(
      n = n(),
      MAE = mean(error_abs),
      RMSE = sqrt(mean(error_cuadrado)),
      .groups = "drop"
    )
  
  print(prueba_por_estacion)
  
  
  #### B. ERRORES POR TEMPORADA ####
  
  prueba_por_temporada <- diagnostico_prueba %>%
    group_by(estacion_objetivo, Modelo) %>%
    summarise(
      n = n(),
      MAE = mean(error_abs),
      RMSE = sqrt(mean(error_cuadrado)),
      .groups = "drop"
    )
  
  print(prueba_por_temporada)
  
  
  #### C. ERRORES EN EPISODIOS ALTOS ####
  
  prueba_episodios <- diagnostico_prueba %>%
    group_by(episodio, Modelo) %>%
    summarise(
      n = n(),
      MAE = mean(error_abs),
      RMSE = sqrt(mean(error_cuadrado)),
      .groups = "drop"
    )
  
  print(prueba_episodios)
  
  
  #### D. COMPARACION POR ESTACION ####
  
  # Comparar Lasso con persistencia en cada estacion.
  
  comparacion_estaciones_prueba <- prueba_por_estacion %>%
    dplyr::select(Station, Modelo, MAE) %>%
    pivot_wider(
      names_from = Modelo,
      values_from = MAE
    ) %>%
    mutate(
      diferencia_MAE = pred_M1 - pred_persistencia
    )
  
  print(comparacion_estaciones_prueba)
  
  # Contar estaciones donde cada procedimiento mejora.
  
  resumen_estaciones_prueba <- comparacion_estaciones_prueba %>%
    summarise(
      estaciones_total = n(),
      mejora_lasso = sum(diferencia_MAE < 0),
      mejora_persistencia = sum(diferencia_MAE > 0),
      empates = sum(diferencia_MAE == 0)
    )
  
  print(resumen_estaciones_prueba)
  
  
  #### E. FIGURA DE ERRORES POR TEMPORADA ####
  
  figura_temporada_prueba <- prueba_por_temporada %>%
    mutate(
      Modelo = recode(
        Modelo,
        "pred_M1" = "Lasso",
        "pred_arbol" = "Arbol podado",
        "pred_bosque" = "Bosque aleatorio",
        "pred_persistencia" = "Persistencia"
      )
    ) %>%
    ggplot(aes(
      x = estacion_objetivo,
      y = MAE,
      color = Modelo,
      group = Modelo
    )) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    labs(
      title = "Error de prediccion por temporada en prueba",
      x = "Estacion del año",
      y = "MAE (µg/m³)",
      color = "Procedimiento"
    ) +
    theme_minimal()
  
  print(figura_temporada_prueba)
  
  # Guardar figura para el informe.
  
  ggsave(
    "figura_temporada_prueba.pdf",
    plot = figura_temporada_prueba,
    width = 8,
    height = 4.5
  )
  
  message(
    "Prueba evaluada sobre ",
    nrow(prueba_final),
    " observaciones completas."
  )
}
