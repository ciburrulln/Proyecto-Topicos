library(glmnet)

formula_m1 <- pm25_futuro ~ pm25_hoy +
  pm25_lag1 + pm25_lag7 + Event.Type.All


variables_m1 <- all.vars(formula_m1)

entrenamiento_m1 <- entrenamiento[, variables_m1, drop = FALSE]
reserva_m1 <- reserva[, variables_m1, drop = FALSE]

# Asegurar que Event.Type.All sea una variable categorica
# y tenga los mismos niveles en ambos conjuntos

niveles_evento <- unique(c(
  as.character(entrenamiento_m1$Event.Type.All),
  as.character(reserva_m1$Event.Type.All)
))

niveles_evento <- niveles_evento[!is.na(niveles_evento)]

entrenamiento_m1$Event.Type.All <- factor(
  entrenamiento_m1$Event.Type.All,
  levels = niveles_evento
)

reserva_m1$Event.Type.All <- factor(
  reserva_m1$Event.Type.All,
  levels = niveles_evento
)

# Eliminar filas con NA

entrenamiento_m1 <- na.omit(entrenamiento_m1)
reserva_m1 <- na.omit(reserva_m1)



# 4. REGRESION LINEAL


modelo_lm <- lm(
  formula_m1,
  data = entrenamiento_m1
)

summary(modelo_lm)

# Predicciones sobre los datos de reserva

pred_lm <- predict(
  modelo_lm,
  newdata = reserva_m1
)



# 5. MATRICES PARA LASSO Y RIDGE


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

# Comprobar que las matrices tengan las mismas columnas

stopifnot(identical(colnames(X_train), colnames(X_test)))



# 6. LASSO


set.seed(3407)

# Validacion cruzada

cv_lasso <- cv.glmnet(
  X_train,
  y_train,
  alpha = 1,
  nfolds = 10
)

# Modelo final

m1_lasso <- glmnet(
  X_train,
  y_train,
  alpha = 1,
  lambda = cv_lasso$lambda.1se
)

# Predicciones

pred_lasso <- as.numeric(
  predict(m1_lasso, newx = X_test)
)



# 7. RIDGE


set.seed(3407)

# Validacion cruzada

cv_ridge <- cv.glmnet(
  X_train,
  y_train,
  alpha = 0,
  nfolds = 10
)

# Modelo final

m2_ridge <- glmnet(
  X_train,
  y_train,
  alpha = 0,
  lambda = cv_ridge$lambda.1se
)

# Predicciones

pred_ridge <- as.numeric(
  predict(m2_ridge, newx = X_test)
)



# 8. FUNCIONES DE EVALUACION


mae <- function(real, pred) {
  mean(abs(real - pred), na.rm = TRUE)
}

mse <- function(real, pred) {
  mean((real - pred)^2, na.rm = TRUE)
}

rmse <- function(real, pred) {
  sqrt(mse(real, pred))
}



# COMPARACION DE LOS MODELOS


resultados <- data.frame(
  
  Modelo = c(
    "Regresion Lineal",
    "Lasso",
    "Ridge"
  ),
  
  MAE = c(
    mae(y_test, pred_lm),
    mae(y_test, pred_lasso),
    mae(y_test, pred_ridge)
  ),
  
  RMSE = c(
    rmse(y_test, pred_lm),
    rmse(y_test, pred_lasso),
    rmse(y_test, pred_ridge)
  )
)

print(resultados)