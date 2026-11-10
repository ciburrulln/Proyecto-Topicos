library(glmnet)

formula_m1 <- pm25_futuro ~ pm25_hoy +
  pm25_lag1 + pm25_lag7 + Event.Type + ultima_conc + antiguedad_dias

variables_m1 <- all.vars(formula_m1)

entrenamiento_m1 <- entrenamiento[, variables_m1, drop = FALSE]
reserva_m1 <- reserva[, variables_m1, drop = FALSE]

niveles_evento <- unique(as.character(entrenamiento_m1$Event.Type))
niveles_evento <- niveles_evento[!is.na(niveles_evento)]

entrenamiento_m1$Event.Type <- factor(
  entrenamiento_m1$Event.Type,
  levels = niveles_evento
)

reserva_m1$Event.Type <- factor(
  reserva_m1$Event.Type,
  levels = niveles_evento
)

reserva_m1 <- reserva_m1[
  !is.na(reserva_m1$Event.Type),
  ,
  drop = FALSE
]

entrenamiento_m1 <- na.omit(entrenamiento_m1)
reserva_m1 <- na.omit(reserva_m1)

modelo_lm <- lm(
  formula_m1,
  data = entrenamiento_m1
)

pred_lm <- predict(
  modelo_lm,
  newdata = reserva_m1
)

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

stopifnot(identical(colnames(X_train), colnames(X_test)))

n <- nrow(X_train)

nfolds <- 10

indices_cv <- vector("list", nfolds)

tamano_validacion <- floor(n / (nfolds + 1))

for (i in seq_len(nfolds)) {
  
  inicio <- i * tamano_validacion + 1
  fin <- min((i + 1) * tamano_validacion, n)
  
  indices_cv[[i]] <- seq(inicio, fin)
}

foldid <- rep(NA_integer_, n)

for (i in seq_len(nfolds)) {
  foldid[indices_cv[[i]]] <- i
}

foldid <- foldid[!is.na(foldid)]

X_cv <- X_train[seq_along(foldid), , drop = FALSE]
y_cv <- y_train[seq_along(foldid)]

set.seed(3407)

cv_lasso <- cv.glmnet(
  X_cv,
  y_cv,
  alpha = 1,
  foldid = foldid,
  type.measure = "mae",
  standardize = TRUE
)

pred_lasso <- as.numeric(
  predict(
    cv_lasso,
    newx = X_test,
    s = "lambda.1se"
  )
)

set.seed(3407)

cv_ridge <- cv.glmnet(
  X_cv,
  y_cv,
  alpha = 0,
  foldid = foldid,
  type.measure = "mae",
  standardize = TRUE
)

pred_ridge <- as.numeric(
  predict(
    cv_ridge,
    newx = X_test,
    s = "lambda.1se"
  )
)

mae <- function(real, pred) {
  mean(abs(real - pred), na.rm = TRUE)
}

mse <- function(real, pred) {
  mean((real - pred)^2, na.rm = TRUE)
}

rmse <- function(real, pred) {
  sqrt(mse(real, pred))
}


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

