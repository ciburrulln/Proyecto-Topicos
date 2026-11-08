library(dplyr)
library(tidyr)

# Preparar datos de entrenamiento y prueba sin NAs en los predictores clave

train_clean <- datos_entrenamiento %>%
  filter(!is.na(pm25_prom_30d), !is.na(pm25_t)) %>%
  drop_na(Elevation, Land.Use, Location.Setting)

test_clean <- datos_prueba %>%
  filter(!is.na(pm25_prom_30d), !is.na(pm25_t)) %>%
  drop_na(Elevation, Land.Use, Location.Setting)


library(glmnet)

# Definir la fórmula del modelo

formula_m1 <- target_pm25_t1 ~ pm25_t + pm25_prom_7d + pm25_prom_30d +
  dias_antiguedad + Elevation + Latitude + Longitude + mes +
  dia_semana + Land.Use + Location.Setting

# Crear matrices de diseño (matriz X y vector Y)

X_train <- model.matrix(formula_m1, data = train_clean)[, -1]
y_train <- train_clean$target_pm25_t1

X_test <- model.matrix(formula_m1, data = test_clean)[, -1]
y_test <- test_clean$target_pm25_t1

# Validación cruzada para elegir lambda en Lasso (alpha = 1) o Ridge (alpha = 0)

set.seed(3407)

cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)

# Modelo final M1 con lambda.1se (o lambda.min)

m1_lasso <- glmnet(
  X_train,
  y_train,
  alpha = 1,
  lambda = cv_lasso$lambda.1se
)

# Predicción en el conjunto de reserva (test)

pred_m1 <- predict(m1_lasso, newx = X_test)


library(mgcv)

# GAM con funciones de suavizado (splines) en variables continuas

m2_gam <- gam(
  target_pm25_t1 ~
    s(pm25_t, k = 10) +
    s(pm25_prom_7d, k = 10) +
    s(pm25_prom_30d, k = 10) +
    s(Elevation, k = 5) +
    s(Latitude, Longitude, k = 15) +
    mes + dia_semana + Location.Setting,
  data = train_clean,
  method = "REML"
)

# Predicción M2

pred_m2 <- predict(m2_gam, newdata = test_clean)


library(rpart)

# Árbol completo y poda por costo de complejidad (cp)

arbol_full <- rpart(
  formula_m1,
  data = train_clean,
  control = rpart.control(cp = 0.0001)
)

cp_optimo <- arbol_full$cptable[
  which.min(arbol_full$cptable[, "xerror"]),
  "CP"
]

m2_arbol <- prune(arbol_full, cp = cp_optimo)

# Predicción M2

pred_m2 <- predict(m2_arbol, newdata = test_clean)


library(ranger)

# Ajuste de Random Forest

set.seed(3407)

m3_rf <- ranger(
  formula = formula_m1,
  data = train_clean,
  num.trees = 500,
  mtry = floor(sqrt(ncol(X_train))), # Regla estándar para mtry
  min.node.size = 10,
  importance = "impurity",
  num.threads = parallel::detectCores() # Uso eficiente de CPU
)

# Predicción M3

pred_m3 <- predict(m3_rf, data = test_clean)$predictions


# Función auxiliar para calcular métricas

calcular_metricas <- function(real, pred, nombre_modelo) {
  
  mae <- mean(abs(real - pred), na.rm = TRUE)
  
  rmse <- sqrt(mean((real - pred)^2, na.rm = TRUE))
  
  tibble(
    Modelo = nombre_modelo,
    MAE = mae,
    RMSE = rmse
  )
}

# Tabla de resultados finales

tabla_comparativa <- bind_rows(
  resumen_baselines, # Creados en el paso anterior
  
  calcular_metricas(y_test, pred_m1, "M1: Lasso (glmnet)"),
  
  calcular_metricas(
    test_clean$target_pm25_t1,
    pred_m2,
    "M2: GAM / Árbol Podado"
  ),
  
  calcular_metricas(
    test_clean$target_pm25_t1,
    pred_m3,
    "M3: Random Forest (ranger)"
  )
)

print(tabla_comparativa)