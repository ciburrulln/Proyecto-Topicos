library(rpart)
library(mgcv)
library(splines)

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

modelo_arbol <- rpart(
  pm25_futuro ~ pm25_hoy +
    pm25_lag1 + pm25_lag7 + Event.Type.All,
  data = entrenamiento_m3,
  method = "anova",
  control = rpart.control(
    cp = 0.001,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 10,
    xval = 10
  )
)

cp_optimo <- modelo_arbol$cptable[
  which.min(modelo_arbol$cptable[, "xerror"]),
  "CP"
]

arbol_podado <- prune(
  modelo_arbol,
  cp = cp_optimo
)

pred_arbol <- predict(
  arbol_podado,
  newdata = reserva_m3
)

modelo_splines <- lm(
  pm25_futuro ~
    ns(pm25_hoy, df = 4) +
    ns(pm25_lag1, df = 4) +
    ns(pm25_lag7, df = 4) +
    Event.Type.All,
  data = entrenamiento_m3
)

pred_splines <- predict(
  modelo_splines,
  newdata = reserva_m3
)

modelo_gam <- gam(
  pm25_futuro ~
    s(pm25_hoy, k = 5) +
    s(pm25_lag1, k = 5) +
    s(pm25_lag7, k = 5) +
    Event.Type.All,
  data = entrenamiento_m3,
  method = "REML",
  select = TRUE
)

pred_gam <- predict(
  modelo_gam,
  newdata = reserva_m3
)

resultados_m3 <- data.frame(
  
  Modelo = c(
    "Arbol podado",
    "Splines",
    "GAM"
  ),
  
  MAE = c(
    mae(reserva_m3$pm25_futuro, pred_arbol),
    mae(reserva_m3$pm25_futuro, pred_splines),
    mae(reserva_m3$pm25_futuro, pred_gam)
  ),
  
  RMSE = c(
    rmse(reserva_m3$pm25_futuro, pred_arbol),
    rmse(reserva_m3$pm25_futuro, pred_splines),
    rmse(reserva_m3$pm25_futuro, pred_gam)
  )
)

print(resultados_m3)

summary(modelo_gam)

plot(arbol_podado)
text(arbol_podado, use.n = TRUE, cex = 0.7)

plot(modelo_gam, pages = 1, residuals = TRUE)