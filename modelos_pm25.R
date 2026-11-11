
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
# Se incluye Event.Type del dia de origen (t), no del dia futuro (t+1).
# IMPORTANTE: justificar que Event.Type estaba disponible al predecir.
pm25_diario <- pm25_diario %>%
  mutate(Date.Local = as.Date(Date.Local))

niveles_estacion <- pm25_diario %>%
  pull(Station) %>% as.character() %>% unique() %>% sort()

niveles_estacion_ano <- pm25_diario %>%
  pull(estacion_ano) %>% as.character() %>% unique() %>% sort()

# Factor de eventos: obtener niveles de los casos de desarrollo
# con pm25_hoy y respuesta observados, sin examinar respuestas de prueba.
niveles_evento <- pm25_diario %>%
  filter(Date.Local + days(1) <= as.Date("2022-12-31"),
         !is.na(pm25_hoy), !is.na(pm25_futuro)) %>%
  pull(Event.Type) %>% as.character() %>% unique() %>% sort()

# No está ultima_conc ni antiguedad_dias dado que usamos solo 
# las observaciones completas

predictores <- c(
  "Station", "Event.Type",
  "pm25_hoy", "pm25_lag1",
  "mes_futuro", "mes",
  "estacion_ano"
)
variables_completas <- c("pm25_futuro", predictores)

# Se mantiene el diagnostico por ESTACION DEL ANO DEL DIA OBJETIVO.
# Esto NO constituye un patron estacional de referencia.
datos_preparados <- pm25_diario %>%
  mutate(
    fecha_objetivo = Date.Local + days(1),
    Station = factor(as.character(Station), levels = niveles_estacion),
    Event.Type = factor(as.character(Event.Type), levels = niveles_evento),
    mes = factor(as.character(mes), levels = as.character(1:12)),
    mes_futuro = factor(as.character(mes_futuro), levels = as.character(1:12)),
    estacion_ano = factor(as.character(estacion_ano),
                          levels = c("Invierno", "Primavera", "Verano", "Otoño")),
    estacion_objetivo = case_when(
      month(fecha_objetivo) %in% 3:5 ~ "Primavera",
      month(fecha_objetivo) %in% 6:8 ~ "Verano",
      month(fecha_objetivo) %in% 9:11 ~ "Otoño",
      TRUE ~ "Invierno"
    ),
    estacion_objetivo = factor(estacion_objetivo,
                               levels = c("Invierno", "Primavera", "Verano", "Otoño"))
  ) %>%
  filter(fecha_objetivo >= as.Date("2019-01-01"),
         fecha_objetivo <= as.Date("2024-12-31"))

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
formula_modelo <- pm25_futuro ~ Station + Event.Type + pm25_hoy +
  pm25_lag1 + mes_futuro + mes + estacion_ano

matriz_x <- function(datos, formula = formula_modelo) {
  model.matrix(formula, data = datos)[, -1, drop = FALSE]
}

metricas <- function(y, pred) {
  tibble(n = length(y),
         MAE = mean(abs(y - pred)),
         RMSE = sqrt(mean((y - pred)^2)))
}

# 2. LOS TRES BLOQUES TEMPORALES EXISTENTES 
# La fecha de la respuesta (t+1) determina la particion.
particiones_modelo <- lapply(bloques, function(b) {
  ent <- datos_modelo %>%
    filter(fecha_objetivo >= b$inicio_train,
           fecha_objetivo < b$inicio_val)
  
  val <- datos_modelo %>%
    filter(fecha_objetivo >= b$inicio_val,
           fecha_objetivo <= b$fin_val)
  
  if (nrow(ent) == 0 || nrow(val) == 0) {
    stop("Particion sin observaciones: revisar fechas y cobertura.")
  }
  if (max(ent$fecha_objetivo) >= min(val$fecha_objetivo)) {
    stop("La respuesta de entrenamiento invade la validacion.")
  }
  list(entrenamiento = ent, validacion = val)
})

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
    ajuste <- glmnet(x_ent, ent$pm25_futuro, alpha = a,
                     lambda = lambdas, standardize = TRUE)
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
grid_bosque <- expand.grid(mtry = c(2, 4, 6),
                           min.node.size = c(10, 30))

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
                      alpha = mejor_m1$alpha, lambda = lambdas,
                      standardize = TRUE)
  pred_m1 <- as.numeric(predict(ajuste_m1, newx = matriz_x(val),
                                s = mejor_m1$lambda))
  
  arbol_grande <- construir_arbol(ent)
  arbol_podado <- prune(arbol_grande, cp = mejor_arbol$cp)
  pred_arbol <- as.numeric(predict(arbol_podado, newdata = val))
  
  bosque <- construir_bosque(ent, mejor_bosque$mtry,
                             mejor_bosque$min.node.size, semilla = semilla)
  pred_bosque <- as.numeric(predict(bosque, data = val)$predictions)
  
  tibble(
    Station = val$Station,
    fecha_objetivo = val$fecha_objetivo,
    estacion_objetivo = val$estacion_objetivo,
    pm25_futuro = val$pm25_futuro,
    pred_persistencia = val$ultima_conc,
    pred_M1 = pred_m1,
    pred_arbol = pred_arbol,
    pred_bosque = pred_bosque
  )
}

# Conservar las mismas semillas usadas al evaluar la configuracion RF elegida.
indice_mejor_bosque <- which(
  grid_bosque$mtry == mejor_bosque$mtry &
    grid_bosque$min.node.size == mejor_bosque$min.node.size
)[1]

pred_validacion <- bind_rows(lapply(seq_along(particiones_modelo), function(i) {
  p <- particiones_modelo[[i]]
  predecir_todos(p$entrenamiento, p$validacion,
                 semilla = SEMILLA + 100 * i + indice_mejor_bosque) %>%
    mutate(Bloque = names(particiones_modelo)[i])
}))

# 7. METRICAS COMPARABLES: MISMAS FILAS PARA LOS CUATRO METODOS
nombres_pred <- c(
  "Referencia: persistencia" = "pred_persistencia",
  "M1: regresion penalizada" = "pred_M1",
  "M2: arbol podado" = "pred_arbol",
  "M3: bosque aleatorio" = "pred_bosque"
)

comparar_modelos <- function(datos) {
  bind_rows(lapply(names(nombres_pred), function(nombre) {
    variable <- nombres_pred[[nombre]]
    metricas(datos$pm25_futuro, datos[[variable]]) %>%
      mutate(Modelo = nombre)
  })) %>%
    dplyr::select(Modelo, n, MAE, RMSE) %>%
    mutate(diferencia_MAE_persistencia =
             MAE - MAE[Modelo == "Referencia: persistencia"])
}

comparacion_validacion <- comparar_modelos(pred_validacion) %>%
  arrange(MAE)
print(comparacion_validacion)

# Comparar tambien cada ventana por separado.
# La dispersion de ventanas solapadas NO equivale a un IC independiente.
comparacion_bloques <- bind_rows(lapply(names(particiones_modelo), function(nombre) {
  comparar_modelos(filter(pred_validacion, Bloque == nombre)) %>%
    mutate(Bloque = nombre)
}))
print(comparacion_bloques)

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

# 8. DIAGNOSTICO POR ESTACION, ESTACION DEL ANO Y EPISODIOS ALTOS
# Umbral obtenido UNICAMENTE con respuestas de desarrollo (2019-2022).
umbral_alto <- datos_modelo %>%
  filter(fecha_objetivo <= as.Date("2022-12-31")) %>%
  pull(pm25_futuro) %>%
  quantile(probs = 0.90, names = FALSE)

diagnostico_largo <- pred_validacion %>%
  mutate(episodio = if_else(pm25_futuro >= umbral_alto,
                            "Concentracion alta", "Resto")) %>%
  pivot_longer(cols = all_of(unname(nombres_pred)),
               names_to = "Modelo", values_to = "Prediccion") %>%
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
figura_temporada <- ggplot(errores_temporada,
                           aes(x = estacion_objetivo, y = MAE,
                               color = Modelo, group = Modelo)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "Error de prediccion de PM2.5 por temporada",
       x = "Estacion del ano del dia objetivo",
       y = "MAE (microgramos/m3)", color = "Modelo") +
  theme_minimal()
print(figura_temporada)

# 9. DOS ANALISIS DE SENSIBILIDAD EN DESARROLLO
# (A) Representacion: sin pm25_lag1 vs con pm25_lag1.
formula_corta <- update(formula_modelo, . ~ . - pm25_lag1)

sensibilidad_historia <- bind_rows(lapply(names(particiones_modelo), function(nombre) {
  ent <- particiones_modelo[[nombre]]$entrenamiento
  val <- particiones_modelo[[nombre]]$validacion
  ajuste <- glmnet(matriz_x(ent, formula_corta), ent$pm25_futuro,
                   alpha = mejor_m1$alpha, lambda = lambdas,
                   standardize = TRUE)
  pred_corta <- as.numeric(predict(ajuste,
                                   newx = matriz_x(val, formula_corta),
                                   s = mejor_m1$lambda))
  pred_larga <- filter(pred_validacion, Bloque == nombre)$pred_M1
  tibble(Bloque = nombre, n = nrow(val),
         MAE_historia_corta = mean(abs(val$pm25_futuro - pred_corta)),
         MAE_historia_larga = mean(abs(val$pm25_futuro - pred_larga)))
})) %>%
  mutate(diferencia_corta_menos_larga =
           MAE_historia_corta - MAE_historia_larga)
print(sensibilidad_historia)

# (B) Poblacion: cambia la comparacion si no contamos episodios altos.
# Se diagnostica el efecto; NO se vuelve a seleccionar el modelo.
sensibilidad_poblacion <- bind_rows(
  comparar_modelos(pred_validacion) %>% mutate(Escenario = "Todos los dias"),
  comparar_modelos(filter(pred_validacion, pm25_futuro < umbral_alto)) %>%
    mutate(Escenario = "Sin episodios altos")
) %>%
  dplyr::select(Escenario, everything()) %>%
  arrange(Escenario, MAE)
print(sensibilidad_poblacion)

# 10. PRUEBA FINAL SELLADA (2023-2024)
# Dejar FALSE mientras comparan modelos y cierran las decisiones.
# Cambiar a TRUE UNA SOLA VEZ cuando hayan congelado el procedimiento.
EVALUAR_PRUEBA_FINAL <- FALSE

if (EVALUAR_PRUEBA_FINAL) {
  entrenamiento_final <- datos_modelo %>%
    filter(fecha_objetivo >= as.Date("2019-01-01"),
           fecha_objetivo <= as.Date("2022-12-31"))
  
  prueba_final <- datos_modelo %>%
    filter(fecha_objetivo >= as.Date("2023-01-01"),
           fecha_objetivo <= as.Date("2024-12-31"))
  
  if (nrow(prueba_final) == 0) {
    stop("No hay casos completos en prueba final.")
  }
  
  pred_prueba <- predecir_todos(entrenamiento_final, prueba_final,
                                semilla = 9999)
  
  comparacion_prueba <- comparar_modelos(pred_prueba) %>% arrange(MAE)
  print(comparacion_prueba)
  
  # Errores por estacion y temporada en prueba, sin cambiar parametros.
  diagnostico_prueba <- pred_prueba %>%
    pivot_longer(cols = all_of(unname(nombres_pred)),
                 names_to = "Modelo", values_to = "Prediccion") %>%
    mutate(error_abs = abs(pm25_futuro - Prediccion))
  
  prueba_por_estacion <- diagnostico_prueba %>%
    group_by(Station, Modelo) %>%
    summarise(n = n(), MAE = mean(error_abs), .groups = "drop")
  
  prueba_por_temporada <- diagnostico_prueba %>%
    group_by(estacion_objetivo, Modelo) %>%
    summarise(n = n(), MAE = mean(error_abs), .groups = "drop")
  
  print(prueba_por_estacion)
  print(prueba_por_temporada)
  
  prueba_episodios <- diagnostico_prueba %>%
    mutate(episodio = if_else(pm25_futuro >= umbral_alto,
                              "Concentracion alta", "Resto")) %>%
    group_by(episodio, Modelo) %>%
    summarise(n = n(), MAE = mean(error_abs), .groups = "drop")
  print(prueba_episodios)
  
  message("Prueba evaluada solo sobre casos completos: ", nrow(prueba_final))
}
