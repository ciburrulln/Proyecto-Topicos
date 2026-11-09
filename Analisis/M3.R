library(randomForest)

variables_m3 <- c(
  "pm25_futuro",
  "pm25_hoy",
  "pm25_lag1",
  "pm25_lag7",
  "Event.Type.All",
  "ultima_conc",
  "antiguedad_dias"
)


entrenamiento_m3 <- entrenamiento[, variables_m3, drop = FALSE]
reserva_m3 <- reserva[, variables_m3, drop = FALSE]

niveles_evento <- unique(as.character(entrenamiento_m3$Event.Type.All))
niveles_evento <- niveles_evento[!is.na(niveles_evento)]

entrenamiento_m3$Event.Type.All <- factor(
  entrenamiento_m3$Event.Type.All,
  levels = niveles_evento
)

reserva_m3$Event.Type.All <- factor(
  reserva_m3$Event.Type.All,
  levels = niveles_evento
)

reserva_m3 <- reserva_m3[
  !is.na(reserva_m3$Event.Type.All),
  ,
  drop = FALSE
]

entrenamiento_m3 <- na.omit(entrenamiento_m3)
reserva_m3 <- na.omit(reserva_m3)

set.seed(3407)

modelo_rf <- randomForest(
  pm25_futuro ~ pm25_hoy +
    pm25_lag1 + pm25_lag7 + Event.Type.All,
  data = entrenamiento_m3,
  ntree = 500,
  mtry = 2,
  nodesize = 5,
  importance = TRUE
)

pred_rf <- predict(
  modelo_rf,
  newdata = reserva_m3
)

resultados_m3 <- data.frame(
  
  Modelo = "Random Forest",
  
  MAE = mae(
    reserva_m3$pm25_futuro,
    pred_rf
  ),
  
  RMSE = rmse(
    reserva_m3$pm25_futuro,
    pred_rf
  ))

print(resultados_m3)

print(modelo_rf)

importance(modelo_rf)

varImpPlot(modelo_rf)