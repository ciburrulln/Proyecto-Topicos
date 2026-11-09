
#### ¿Por qué no usamos la duración 1 HR en estas 4 estaciones? ####

#### Revisar estación 65-8001 ####

pm25_diario_original %>%
  filter(Station == "65-8001") %>%
  group_by(POC, Sample.Duration, Method.Code) %>%
  summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de POC el 2022 para el método 1 HR, pero
# por 26 observaciones más no conviene cambiarlo

#### Revisar estación 71-27 ####

pm25_diario_original %>%
  filter(Station == "71-27") %>%
  group_by(POC, Sample.Duration, Method.Code) %>%
  summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de método el 2022 para la duración 1 HR 
# PERO tiene más observaciones la duración 24 HRS
# 2120 > 1909 (CASI 200 OBSERVACIONES MÁS)


#### Revisar estación 37-4008 ####

pm25_diario_original %>%
  filter(Station == "37-4008") %>%
  group_by(POC, Sample.Duration, Method.Code) %>%
  summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = n_distinct(Date.Local),
    .groups = "drop"
  )
# La duración 1 hr llega hasta el 2021

#### Revisar estación 37-1103 ####

pm25_diario_original %>%
  filter(Station == "37-1103") %>%
  group_by(POC, Sample.Duration, Method.Code) %>%
  summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = n_distinct(Date.Local),
    .groups = "drop"
  )
# La duración 24 hrs tiene más de 1000 observaciones más que 
# la duración 1 hr
