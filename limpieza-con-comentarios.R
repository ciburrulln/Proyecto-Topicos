if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(here, tidyverse, lubridate, dplyr)

#### CARGAR BASES DE DATOS ####

# Verificar directorio raíz del proyecto
here()

# Cargar bases de datos
pm25_diario <- read.csv(here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here("datos", "diccionario.csv"))

sitios <- read.csv(here("datos", "sitios.csv"))

#### LIMPIEZA DE VARIABLES ####

# No tiene NA
apply(is.na(pm25_diario), 2, sum)

# Variables constantes
sapply(pm25_diario, function(x) length(unique(x)) == 1)


# Eliminar las variables redundantes pues todas las mediciones son en California
# y solo estamos calculando pm25. 
# Date of Last Change no entrega información relevante. 
# Method.Name da la misma información que Method.Code.
# Pollutant Standard da la misma información que Sample.Duration 
table(pm25_diario$Pollutant.Standard, pm25_diario$Sample.Duration)

pm25_diario <- pm25_diario %>%
  dplyr::select(-State.Code, -State.Name, 
         -Parameter.Code, -Parameter.Name, -Units.of.Measure,
         -Date.of.Last.Change, -Method.Name, -Pollutant.Standard)

# Eliminar variables con fuga de información, en este caso eliminamos 
# las variables que registran el valor máximo durante ese día,
# la hora local a la que ocurrió y el índice de calidad de aire
pm25_diario <- pm25_diario %>%
  dplyr::select(-X1st.Max.Value, -X1st.Max.Hour, -AQI)

# Información que ya existe en sitios.cvs y la cruzaremos posteriormente
pm25_diario <- pm25_diario %>%
  dplyr::select(-Latitude, -Longitude, -Datum, -Local.Site.Name, -Address, 
         -County.Name, -City.Name, -CBSA.Name)


# County.Code y Site.Num son las variables que identifican la estación,
# el date local da justo la observación de
# una fecha específica en una estación


# Darle a la fecha formato de fecha
# Crear variable para identificar las estaciones
# Eliminar las variables County.Code y Site.Num
pm25_diario <- pm25_diario %>%
  mutate(
    across(Date.Local, as.Date),
    Station = paste(County.Code, Site.Num, sep = "-")
  ) %>%
  relocate(Station, Method.Code) %>%
  dplyr::select(-County.Code, -Site.Num) %>%
  mutate(Arithmetic.Mean = as.numeric(Arithmetic.Mean))


#### LIMPIEZA DE OBSERVACIONES ####

# Ver las combinaciones más comunes de instrumento, duración y método
# Preocuparse de no promediar indiscriminadamente resúmenes incompatibles

names(pm25_diario)

# Casi todas las estaciones cuentan con la misma ventana de tiempo
# La estación 47-3 no tiene días en el 2024, no la eliminamos pero lo auditamos
# Tendremos que tener cuidado en la validación
# Mientras a la estación 37-4008 le faltan solo 8 días para completar el año

dias_totales <- as.numeric(max(pm25_diario$Date.Local) - 
                             min(pm25_diario$Date.Local)) + 1
info_estacion <- pm25_diario %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            total_observaciones = n(),
            dias_sin_medicion = dias_totales - total_dias)



# Todas las estaciones tienen más de una máquina 
# o más de una forma de tomar las muestras, por lo que es importante
# elegir la mejor combinación para cada estación

maquinas_por_estacion <- pm25_diario %>%
  group_by(Station) %>%
  summarise(
    poc_observados = n_distinct(POC),
    dur_observadas = n_distinct(Sample.Duration),
    met_observados = n_distinct(Method.Code),
    combinaciones_totales = n_distinct(paste(POC, Sample.Duration, Method.Code))
  )

#### IDENTIFICAR LAS COMBINACIONES ####

combinaciones <- pm25_diario %>%
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  summarise(
    n_registros = n(),
    n_fechas = n_distinct(Date.Local),
    media_porcentaje_obs = mean(Observation.Percent),
    .groups = "drop"
  ) %>%
  arrange(Station, desc(n_fechas))

# Vemos que cada estación tiene más de una forma de medir la contaminación, por
# lo que debemos elegir una para que sea comparable nuestra predicción

#### ELEGIR MEJOR COMBINACIÓN ####
# Solo entre los años 2019-2022

# las mediciones de menos de 18 horas no son significativas
# según la información disponible en el leeme norma epa
pm25_filtrado <- pm25_diario %>%
  filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) |
      (Sample.Duration == "24-HR BLK AVG" & Observation.Count == 1))

mejores_combinaciones_filtradas <- pm25_filtrado %>%
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  summarise(
    dias_observados = n(),
    .groups = "drop_last" # No sé pq pone eso
  ) %>%
  # Ordenar para que la con más cobertura quede en primera fila
  arrange(Station, desc(dias_observados)) 

mejores_combinaciones <- pm25_diario %>%
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  summarise(
    dias_observados = n(),
    .groups = "drop_last" # No sé pq pone eso
  ) %>%
  # Ordenar para que la con más cobertura quede en primera fila
  arrange(Station, desc(dias_observados)) 

# Se observa en mejores_combinaciones_filtradas que los promedios 
# de bloque de 24 horas (24 HRS BLK AVG) coinciden en cantidad
# con los promedios horarios (1 HR) si eliminamos las muestras 
# donde con la duración 1HR se usan menos de 18 horas. 
# Notamos que solo se diferencian por el truncamiento 
# a un decimal aplicado por la EPA.
# Por lo que basta analizar la mejor combinación solo con uno de los dos.
# Nos quedamos con 1HR con las observaciones con más de 18 horas para calcular el promedio
# dado que conserva más decimales en Arithmetic.Mean

table(pm25_diario$Observation.Count, pm25_diario$Sample.Duration)

pm25_diario <- pm25_diario %>%
  filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) )

#### ELEGIR MEJOR COMBINACIÓN ####
# Solo entre los años 2019-2022

mejor_combinacion <- pm25_diario %>%
  filter(year(Date.Local) %in% 2019:2022) %>%
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  summarise(
    dias_observados = n_distinct(Date.Local),
    .groups = "drop"
  ) %>%
  group_by(Station) %>%
  # Priorizar la combinación con más días
  arrange(desc(dias_observados), .by_group = TRUE) %>%
  # Elegir una combinación por estación
  slice_head(n = 1) %>%
  ungroup() %>%
  # Ordenar las estaciones numéricamente
  arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station))
  )

#### ELIMINAR OBSERVACIONES DUPLICADAS USANDO LA MEJOR COMBINACIÓN ####

pm25_diario_original <- pm25_diario

pm25_diario <- pm25_diario %>%
  semi_join(
    mejor_combinacion,
    by = c("Station", "POC", "Sample.Duration", "Method.Code")
  ) %>%
  arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station)),
    Date.Local
  )

# Verificar que no existan duplicados
pm25_diario %>%
  count(Station, Date.Local) %>%
  filter(n > 1)

# Analizamos nuevamente la info de cada estacion 
info_estacion_final <- pm25_diario %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            total_observaciones = n(),
            NAS = dias_totales - total_dias)

#### Revisar estación 31-1004 ####
# Estación con muchos días sin medición

pm25_diario_original %>%
  filter(Station == "31-1004") %>%
  group_by(POC, Sample.Duration, Method.Code) %>%
  summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de Método el 2020! Por eso no contabilizó todo el periodo

# Recuperar las observaciones del método anterior
observaciones_recuperadas <- pm25_diario_original %>%
  filter(
    Station == "31-1004",
    POC == 3,
    Sample.Duration == "1 HOUR",
    Method.Code == 204
  ) %>%
  anti_join(
    pm25_diario %>%
      distinct(Station, Date.Local),
    by = c("Station", "Date.Local")
  )

# Incorporarlas a la base final
pm25_diario <- bind_rows(
  pm25_diario,
  observaciones_recuperadas
) %>%
  arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station)),
    Date.Local
  )

# Verificar que no existan duplicados
pm25_diario %>%
  count(Station, Date.Local) %>%
  filter(n > 1)

# Analizamos nuevamente la info de cada estacion 
info_estacion_final <- pm25_diario %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            total_observaciones = n(),
            dias_sin_medicion = dias_totales - total_dias)

# Borramos información que no es necesaria 
pm25_diario <- pm25_diario %>%
  dplyr::select(-Method.Code, -POC, -Sample.Duration,
         -Observation.Count, -Observation.Percent )

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



#### Agregar días ####

# Días sin medición -------------------------------------------------------

pm25_diario <- pm25_diario %>%
  group_by(Station) %>%
  # Rellena todos los días calendario faltantes entre el inicio y fin de cada estación
  complete(Date.Local = seq.Date(min(as.Date("2019-01-01"), na.rm = TRUE), max(as.Date("2024-12-31"), na.rm = TRUE), by = "day")) %>%
  ungroup()

apply(is.na(pm25_diario), 2, sum)

# Contar los días sin medición (NA) por estación
info_estacion_final <- pm25_diario %>%
  group_by(Station) %>%
  summarise(
    min = min(Date.Local),
    max = max(Date.Local),
    total_dias = n_distinct(Date.Local),
    total_observaciones = n(),
    dias_con_medicion  = sum(!is.na(Arithmetic.Mean)),
    dias_sin_medicion  = sum(is.na(Arithmetic.Mean)),
    porcentaje_faltante = round((dias_sin_medicion / dias_totales) * 100, 2)
  ) %>%
  arrange(desc(dias_sin_medicion)) # Ordena de la estación con más baches a la que menos tiene

# días faltantes totales
sum(info_estacion_final$dias_sin_medicion)
# proporción días faltantes
sum(info_estacion_final$dias_sin_medicion) / (2192*30)
# Aprox el 3%


#Los días sin datos ahora quedan como NA
pm25_diario <- pm25_diario %>%
  group_by(Station) %>%
  complete(Date.Local = seq.Date(min(Date.Local, na.rm = TRUE), max(Date.Local, na.rm = TRUE), by = "day")) %>%
  arrange(Station, Date.Local) %>%
  #Rezagos
  mutate(
    pm25_hoy    = Arithmetic.Mean,
    pm25_lag1   = lag(pm25_hoy, 1),   # Estrictamente el día calendario anterior (o NA si faltaba)
    pm25_lag7   = lag(pm25_hoy, 7),   # Estrictamente 7 días calendario atrás
    pm25_futuro = lead(pm25_hoy, 1)   # Estrictamente el día calendario siguiente
  ) %>%
  dplyr::select(- Arithmetic.Mean) %>%
  ungroup()

sum(is.na(pm25_diario$pm25_hoy)) #NAs totales por dia


# Creación de variable Event.Type sin NA 
pm25_diario <- pm25_diario %>%
  mutate(
    Event.Type = case_when(
      is.na(Event.Type) ~ "Sin medicion",
      trimws(Event.Type) == "" ~ "Sin evento informado",
      Event.Type == "Included" ~ "Evento incluido",
      TRUE ~ Event.Type
    )
  )


# Crear una referencia utilizando solo mediciones pasadas
# También crea la antiguedad en días de esa ultima concentración
# El mes y el mes futuro podrían llegar a ser un buen predictor 
pm25_diario <- pm25_diario %>%
  mutate(
    Date.Local = as.Date(Date.Local),
    mes = factor(month(Date.Local)),
    mes_futuro = factor(month(Date.Local + days(1))),
    ultima_conc = pm25_hoy,
    fecha_ultima = if_else(
      !is.na(pm25_hoy),
      Date.Local,
      as.Date(NA)
    )
  ) %>%
  group_by(Station) %>%
  arrange(Date.Local, .by_group = TRUE) %>%
  fill(ultima_conc, fecha_ultima, .direction = "down") %>%
  mutate(
    antiguedad_dias =
      as.integer(Date.Local - fecha_ultima)
  ) %>%
  ungroup()

# No se modifican los NA originales
# ultima_conc solo se utiliza como predicción de referencia

pm25_diario <- pm25_diario %>%
  relocate(pm25_futuro)

names(pm25_diario)
