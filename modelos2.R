
pacman::p_load(glmnet)


# Data frame para los resultados

resultados <- data.frame(
  bloque = character(),
  modelo = character(),
  lambda = numeric(),
  n = integer(),
  RMSE = numeric(),
  MAE = numeric(),
  MSE = numeric()
)

metricas <- function(y_verdadero, y_predicho){
  n <- length(y_predicho)
  MAE <- mean(abs(y_verdadero - y_predicho))
  MSE <- mean((y_verdadero - y_predicho)^2)
  RMSE <- sqrt(MSE)
  return(c(n = n, MAE = MAE, MSE = MSE, RMSE = RMSE))
}


# Regresión Lasso y Ridge

formula_m1 <- pm25_futuro ~ 
  Station +
  mes_futuro +
  pm25_hoy +
  pm25_lag1 +
  pm25_lag7 +
  ultima_conc +
  antiguedad_dias +
  pm25_hoy_faltante +
  pm25_lag1_faltante +
  pm25_lag7_faltante +
  ultima_conc_faltante +
  antiguedad_dias_faltante

variables_m1 <- all.vars(formula_m1)

lambdas <- c(0.05, 0.2, 1, 3, 10, 30, 100)


for (i in 1:length(particiones)) {
  
  entrenamiento <- particiones[[i]]$entrenamiento
  validacion <- particiones[[i]]$validacion
  
  # Seleccionar variables
  entrenamiento_m1 <- entrenamiento[, variables_m1, drop = FALSE]
  validacion_m1 <- validacion[, variables_m1, drop = FALSE]
  
  # Matrices para glmnet
  x_entrenamiento <- model.matrix(formula_m1, data = entrenamiento_m1)[, -1]
  x_validacion <- model.matrix(formula_m1, data = validacion_m1)[, -1]
  
  y_entrenamiento <- entrenamiento_m1$pm25_futuro
  
  # Valores observados
  observado <- validacion_m1$pm25_futuro
  
  
  # Modelo Lasso
  modelo_lasso <- glmnet(
    x = x_entrenamiento,
    y = y_entrenamiento,
    family = "gaussian",
    alpha = 1,
    lambda = sort(lambdas, decreasing = TRUE),
    standardize = TRUE
  )
  
  for (j in 1:length(lambdas)) {
    
    pred_lasso <- predict(
      modelo_lasso,
      newx = x_validacion,
      s = lambdas[j]
    )
    
    metricas_lasso <- metricas(observado, as.numeric(pred_lasso))
    
    resultados <- rbind(
      resultados,
      data.frame(
        bloque = names(particiones)[i],
        modelo = "Lasso",
        lambda = lambdas[j],
        n = metricas_lasso["n"],
        RMSE = metricas_lasso["RMSE"],
        MAE = metricas_lasso["MAE"],
        MSE = metricas_lasso["MSE"]
      )
    )
  }
  
  
  # Modelo Ridge
  modelo_ridge <- glmnet(
    x = x_entrenamiento,
    y = y_entrenamiento,
    family = "gaussian",
    alpha = 0,
    lambda = sort(lambdas, decreasing = TRUE),
    standardize = TRUE
  )
  
  for (j in 1:length(lambdas)) {
    
    pred_ridge <- predict(
      modelo_ridge,
      newx = x_validacion,
      s = lambdas[j]
    )
    
    metricas_ridge <- metricas(observado, as.numeric(pred_ridge))
    
    resultados <- rbind(
      resultados,
      data.frame(
        bloque = names(particiones)[i],
        modelo = "Ridge",
        lambda = lambdas[j],
        n = metricas_ridge["n"],
        RMSE = metricas_ridge["RMSE"],
        MAE = metricas_ridge["MAE"],
        MSE = metricas_ridge["MSE"]
      )
    )
  }
}

# Resultados finales
print(resultados)
