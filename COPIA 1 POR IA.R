
# Librerías ---------------------------------------------------------------
library(dplyr)
library(tidyr)

datos <- pm25_diario
class(datos$Date.Local)
# 1. Asegurar que la columna sea de tipo fechada
datos$fecha <- as.Date(datos$Date.Local)
##creamos un identificador unico para cada estacion, corrigiendo por 

datos <- datos %>%
  mutate(estacion = paste(State.Code, County.Code, Site.Num, sep = "-")) %>%
  arrange(estacion, fecha)

datos <- datos %>%
  group_by(estacion) %>%
  mutate(
    pm25_hoy    = Arithmetic.Mean,          # Concentración actual de PM2.5 (o la columna numérica que uses)
    pm25_lag1   = lag(pm25_hoy, 1),         # Ayer (t - 1)
    pm25_lag7   = lag(pm25_hoy, 7),         # Hace 7 días (t - 7)
    pm25_futuro = lead(pm25_hoy, 1)         # Mañana (t + 1) -> Nuestra respuesta a predecir
  ) %>%
  ungroup()

saltos_fechas <- datos %>%
  mutate(estacion = paste(State.Code, County.Code, Site.Num, sep = "-")) %>%
  group_by(estacion) %>%
  summarize(
    min_f = min(fecha),
    max_f = max(fecha),
    dias_reales = n(),
    dias_calendario = as.numeric(max_f - min_f) + 1,
    faltan_dias = dias_calendario - dias_reales
  )

print(saltos_fechas)
#vemos que hay 30 fechas donde se encuentran saltos 

datos_calendario_estricto <- datos %>%
  mutate(estacion = paste(State.Code, County.Code, Site.Num, sep = "-")) %>%
  group_by(estacion) %>%
  # Rellena con NA todos los días faltantes entre el mínimo y máximo de cada estación
  complete(fecha = seq.Date(min(fecha, na.rm = TRUE), max(fecha, na.rm = TRUE), by = "day")) %>%
  # Rellenamos de vuelta los códigos de estación para que no queden como NA en los días vacíos
  fill(State.Code, County.Code, Site.Num, .direction = "downup") %>%
  arrange(estacion, fecha) %>%
  
  # 2. Ahora sí, calcular rezagos y respuestas sobre el calendario real
  mutate(
    pm25_hoy    = Arithmetic.Mean,
    pm25_lag1   = lag(pm25_hoy, 1),  # Estrictamente el día calendario anterior (o NA si faltaba)
    pm25_lag7   = lag(pm25_hoy, 7),  # Estrictamente 7 días calendario atrás
    pm25_futuro = lead(pm25_hoy, 1)  # Estrictamente el día calendario siguiente
  ) %>%
  ungroup()

#partición por fechas
#2019-2021 - entrenamiento, 2022 - validación, 2023-2024 - prueba
entrenamiento <- datos %>% filter(fecha >= "2019-01-01" & fecha <= "2021-12-31")
validacion   <- datos %>% filter(fecha >= "2022-01-01" & fecha <= "2022-12-31")
prueba    <- datos %>% filter(fecha >= "2023-01-01" & fecha <= "2024-12-31")

nas_por_columna <- datos %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "total_na") %>%
  mutate(porcentaje_na = round((total_na / nrow(datos)) * 100, 2)) %>%
           arrange(desc(porcentaje_na))

print("--- 1. VALORES FALTANTES POR VARIABLE ---")
view(nas_por_columna)

##solo la variable AQI tiene valores faltantes 
#AQI -> Quedan excluidos como predictores el AQI (instrucciones del trabajo)

summary(datos)
