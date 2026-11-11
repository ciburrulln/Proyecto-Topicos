if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(here, tidyverse, lubridate)
library(dplyr)

#### CARGAR BASES DE DATOS ####

# Verificar directorio raíz del proyecto
here()

# Cargar bases de datos
pm25_diario <- read.csv(here("datos", "pm25_diario.csv"))

#### LIMPIEZA DE VARIABLES ####

pm25_diario <- pm25_diario %>%
  dplyr::select(-State.Code, -State.Name, 
         -Parameter.Code, -Parameter.Name, -Units.of.Measure,
         -Date.of.Last.Change, -Method.Name, -Pollutant.Standard,
         -X1st.Max.Value, -X1st.Max.Hour, -AQI,
         -Latitude, -Longitude, -Datum, -Local.Site.Name, -Address, 
         -County.Name, -City.Name, -CBSA.Name)

pm25_diario <- pm25_diario %>%
  mutate(
    across(Date.Local, as.Date),
    Station = paste(County.Code, Site.Num, sep = "-")
  ) %>%
  relocate(Station, Method.Code) %>%
  dplyr::select(-County.Code, -Site.Num) %>%
  mutate(Arithmetic.Mean = as.numeric(Arithmetic.Mean))


#### LIMPIEZA DE OBSERVACIONES ####
#### ELEGIR MEJOR COMBINACIÓN ####
dias_totales <- as.numeric(max(pm25_diario$Date.Local) - 
                             min(pm25_diario$Date.Local)) + 1

pm25_diario <- pm25_diario %>%
  filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) )

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

# Borramos información que no es necesaria para la predicción
pm25_diario <- pm25_diario %>%
  dplyr::select(-Method.Code, -POC, -Sample.Duration,
         -Observation.Count, -Observation.Percent )


#### Agregar días ####

# Días sin medición 

pm25_diario <- pm25_diario %>%
  group_by(Station) %>%
  # Rellena todos los días calendario faltantes entre el inicio y fin de cada estación
  complete(Date.Local = seq.Date(min(as.Date("2019-01-01"), na.rm = TRUE), max(as.Date("2024-12-31"), na.rm = TRUE), by = "day")) %>%
  ungroup()


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
sum(info_estacion_final$dias_sin_medicion) / (2192*29)
# Aproximadamente el 3%


#Los días sin datos ahora quedan como NA
pm25_diario <- pm25_diario %>%
  group_by(Station) %>%
  complete(Date.Local = seq.Date(min(Date.Local, na.rm = TRUE), max(Date.Local, na.rm = TRUE), by = "day")) %>%
  arrange(Station, Date.Local) %>%
  #Rezagos
  mutate(
    pm25_hoy    = Arithmetic.Mean,
    pm25_lag1   = lag(pm25_hoy, 1),   # Estrictamente el día calendario anterior (o NA si faltaba)
    pm25_futuro = lead(pm25_hoy, 1)   # Estrictamente el día calendario siguiente
  ) %>%
  dplyr::select(- Arithmetic.Mean) %>%
  ungroup()

sum(is.na(pm25_diario$pm25_hoy)) #NAs totales por dia




# Crear una referencia utilizando solo mediciones pasadas ultima_conc
# También crea la antiguedad en días de esa ultima concentración antiguedad_dias
# El mes y el mes futuro podrían llegar a ser un buen predictor 
# Se agrega estacion del año, dia de la semana y año

pm25_diario <- pm25_diario %>%
  mutate(
    Date.Local = as.Date(Date.Local),
    mes = factor(month(Date.Local)),
    ano = year(Date.Local),
    
    # Día de la semana
    dia_semana = wday(
      Date.Local,
      label = TRUE,
      abbr = FALSE,
      week_start = 1,
      locale = "es_ES"
    ),
    
    # Estación del año (hemisferio norte)
    estacion_ano = case_when(
      month(Date.Local) %in% 3:5 ~ "Primavera",
      month(Date.Local) %in% 6:8 ~ "Verano",
      month(Date.Local) %in% 9:11 ~ "Otoño",
      month(Date.Local) %in% c(12, 1, 2) ~ "Invierno"
    ),
    estacion_ano = factor(
      estacion_ano,
      levels = c("Invierno", "Primavera", "Verano", "Otoño")
    ),
    
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
  dplyr::select(-fecha_ultima, -Event.Type) %>%
  ungroup()

# No se modifican los NA originales


pm25_diario <- pm25_diario %>%
  relocate(pm25_futuro)


(65760 - sum(complete.cases(pm25_diario)) ) / 65760
# Observaciones incompletas


