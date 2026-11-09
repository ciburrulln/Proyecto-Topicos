

#PARTICIONES-----------------------------------------------------------------

names(pm25_diario)

entrenamiento <- pm25_diario %>% filter(Date.Local >= "2019-01-01" & Date.Local <= "2021-12-31")
validacion   <- pm25_diario %>% filter(Date.Local >= "2022-01-01" & Date.Local <= "2022-12-31")
reserva    <- pm25_diario %>% filter(Date.Local >= "2023-01-01" & Date.Local <= "2024-12-31")

modelo_lm <- lm(pm25_futuro ~ pm25_hoy + pm25_lag1 + pm25_lag7, data = entrenamiento)
summary(modelo_lm)

#lm con tipo de evento -> es significativo pero no aporta??
modelo_lm1 <- lm(pm25_futuro ~ pm25_hoy + pm25_lag1 + pm25_lag7 + Event.Type , data = entrenamiento)
summary(modelo_lm1)

#lm con tipo de evento sin NA 
modelo_lm2 <- lm(pm25_futuro ~ pm25_hoy + pm25_lag1 + pm25_lag7 + Event.Type.All , data = entrenamiento)
summary(modelo_lm2)


#PARA EL PRIMER MODELO
real <- pm25_diario$pm25_futuro
pred <- predict(modelo_lm)

mae  <- function(real, pred) mean(abs(real - pred), na.rm = TRUE)
rmse <- function(real, pred) sqrt(mean((real - pred)^2, na.rm = TRUE))

##TIENEN DISTINTA LONGITUD real y pred -> POR LOS NA
#mae de regresion lineal
mae(real, pred)

#rmse
rmse(real, pred) #no cambia tanto -> no hay errores tan grandes 



