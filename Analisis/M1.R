
# LIBRERÍAS
library(glmnet)

# DEFINIR LA FÓRMULA
formula_m1 <- pm25_futuro ~ pm25_hoy +
  pm25_lag1 + pm25_lag7 + Event.Type.All

# ELIMINAR NA
# Utilizamos las mismas variables que en el primer modelo

variables_m1 <- all.vars(formula_m1)

entrenamiento_m1 <- na.omit(
  entrenamiento[, variables_m1]
)

reserva_m1 <- na.omit(
  reserva[, variables_m1]
)

# MATRICES DE DISEÑO

X_train <- model.matrix(
  formula_m1,
  data = entrenamiento_m1
)[, -1, drop = FALSE]

y_train <- entrenamiento_m1$pm25_futuro

X_test <- model.matrix(
  formula_m1,
  data = reserva_m1
)[, -1, drop = FALSE]

y_test <- reserva_m1$pm25_futuro

# VALIDACIÓN CRUZADA PARA ELEGIR LAMBDA

set.seed(3407)

cv_lasso <- cv.glmnet(
  X_train,
  y_train,
  alpha = 1,
  nfolds = 10
)

# MODELO FINAL LASSO

m1_lasso <- glmnet(
  X_train,
  y_train,
  alpha = 1,
  lambda = cv_lasso$lambda.1se
)

# PREDICCIONES SOBRE RESERVA

pred_m1 <- as.numeric(
  predict(m1_lasso, newx = X_test)
)

# MÉTRICAS DE EVALUACIÓN

mae <- function(real, pred) {
  mean(abs(real - pred))
}

rmse <- function(real, pred) {
  sqrt(mean((real - pred)^2))
}

# RESULTADOS

mae_lasso <- mae(y_test, pred_m1)
rmse_lasso <- rmse(y_test, pred_m1)

mae_lasso
rmse_lasso



# Ridge


# VALIDACIÓN CRUZADA PARA ELEGIR LAMBDA

set.seed(3407)

cv_ridge <- cv.glmnet(
  X_train,
  y_train,
  alpha = 0,
  nfolds = 10
)

# MODELO FINAL ridge

m2_ridge <- glmnet(
  X_train,
  y_train,
  alpha = 1,
  lambda = cv_ridge$lambda.1se
)

# PREDICCIONES SOBRE RESERVA

pred_m2 <- as.numeric(
  predict(m2_ridge, newx = X_test)
)


# RESULTADOS

mae_ridge <- mae(y_test, pred_m2)
rmse_ridge <- rmse(y_test, pred_m2)

mae_ridge
rmse_ridge


