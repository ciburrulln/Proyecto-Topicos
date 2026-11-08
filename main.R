if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(here, tidyverse, lubridate)
library(dplyr)

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
# y solo estamos calculando pm25. Además Date of Last Change no entrega
# información relevante. Method.Name da la misma información que Method.Code, 
# mientras Pollutant Standard da menos información que Sample.Duration 
pm25_diario <- pm25_diario %>%
  select(-State.Code, -State.Name, 
         -Parameter.Code, -Parameter.Name, -Units.of.Measure,
         -Date.of.Last.Change, -Method.Name, -Pollutant.Standard)

# Eliminar variables con fuga de información, en este caso eliminamos 
# las variables que registran el valor máximo durante ese día,
# la hora local a la que ocurrió y el índice de calidad de aire
pm25_diario <- pm25_diario %>%
  select(-X1st.Max.Value, -X1st.Max.Hour, -AQI)

# Información que ya existe en sitios.cvs y la cruzaremos posteriormente
pm25_diario <- pm25_diario %>%
  select(-Latitude, -Longitude, -Datum, -Local.Site.Name, -Address, 
         -County.Name, -City.Name, -CBSA.Name)


# POC, Sample Duration y Method.Code se eliminarán posteriormente

# County.Code y Site.Num son las variables que identifican la estación,
# el date local da justo la observación de
# una fecha específica en una estación


# Darle a la fecha formato de fecha
# Crear variable para identificar las estaciones
# Eliminar las variables que identificaban
pm25_diario <- pm25_diario %>%
  mutate(
    across(Date.Local, as.Date),
    Station = paste(County.Code, Site.Num, sep = "-")
  ) %>%
  relocate(Station, Method.Code) %>%
  select(-County.Code, -Site.Num) %>%
  mutate(Arithmetic.Mean = as.numeric(Arithmetic.Mean))




#### LIMPIEZA DE OBSERVACIONES ####

# Ver las combinaciones más comunes de instrumento, duración y estándar
# Preocuparse de no promediar indiscriminadamente resúmenes incompatibles

names(pm25_diario)

#### LIMPIEZA DE OBSERVACIONES ####

# Casi todas las estaciones cuentan con la misma ventana de tiempo
# La estación 47-3 no tiene días en el 2024, por lo que la borramos por simplicidad
# Mientras a la estación 37-4008 le faltan solo 8 días para completar el año
# NO SÉ QUE HACER ACÁ
dias_totales <- as.numeric(max(pm25_diario$Date.Local) - 
                             min(pm25_diario$Date.Local)) + 1
info_estacion <- pm25_diario %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            total_observaciones = n(),
            NAS = dias_totales - total_dias)

pm25_diario <- pm25_diario %>%
  dplyr::filter(Station != "47-3")


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

#### ELEGIR MEJOR COMBINACIÓN ####
# Solo entre los años 2019-2022

#### ELEGIR MEJOR COMBINACION ####

pm25_filtrado <- pm25_diario %>%
  filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) |
      (Sample.Duration == "24-HR BLK AVG" & Observation.Count == 1))

mejor_combinacion_filtrada <- pm25_filtrado %>%
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  summarise(
    dias_observados = n(),
    .groups = "drop_last" # No sé pq pone eso
  ) %>%
  # Ordenar para que la con más cobertura quede en primera fila
  arrange(Station, desc(dias_observados))  %>%
  slice_head(n = 1) %>%
  ungroup()

mejor_combinacion <- pm25_diario %>%
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  summarise(
    dias_observados = n(),
    .groups = "drop" # No sé pq pone eso
  ) %>%
  # Ordenar para que la con más cobertura quede en primera fila
  arrange(Station, desc(dias_observados))  %>%
  slice_head(n = 1) %>%
  ungroup()

# Se observa que los promedios de bloque de 24 horas (24 HRS BLK AVG) coinciden en cantidad
# con los promedios horarios (1 HR), si eliminamos las muestras donde con la
# duración 1HR se usan menos de 18 horas, solo se diferencian por el truncamiento 
# a un decimal aplicado por la EPA.
# Por lo que basta analizar la mejor combinación solo con uno de los dos.
# Nos quedamos con 1HR con las observaciones con más de 18 horas para calcular el promedio
# dado que conserva más decimales en Arithmetic.Mean

pm25_diario <- pm25_diario %>%
  filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) )

#### ELEGIR MEJOR COMBINACIÓN ####

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

pm25_final <- pm25_diario %>%
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
pm25_final %>%
  count(Station, Date.Local) %>%
  filter(n > 1)

# Analizamos nuevamente la info de cada estacion 
info_estacion_final <- pm25_final %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            total_observaciones = n(),
            NAS = dias_totales - total_dias)

#### Revisar estación 31-1004 ####

pm25_diario %>%
  filter(Station == "31-1004") %>%
  group_by(POC, Sample.Duration, Method.Code) %>%
  summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de Método el 2020!

# Recuperar las observaciones del método anterior
observaciones_recuperadas <- pm25_diario %>%
  filter(
    Station == "31-1004",
    POC == 3,
    Sample.Duration == "1 HOUR",
    Method.Code == 204
  ) %>%
  anti_join(
    pm25_final %>%
      distinct(Station, Date.Local),
    by = c("Station", "Date.Local")
  )

# Incorporarlas a la base final
pm25_final <- bind_rows(
  pm25_final,
  observaciones_recuperadas
) %>%
  arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station)),
    Date.Local
  )

# Verificar que no existan duplicados
pm25_final %>%
  count(Station, Date.Local) %>%
  filter(n > 1)

# Analizamos nuevamente la info de cada estacion 
info_estacion_final <- pm25_final %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            total_observaciones = n(),
            NAS = dias_totales - total_dias)
