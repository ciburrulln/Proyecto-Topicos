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


# POC, Sample Duration, Pollutant Standard se eliminarán posteriormente

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

names(pm25_diario)




#### LIMPIEZA DE OBSERVACIONES ####

# Ver las combinaciones más comunes de instrumento, duración y estándar
# Preocuparse de no promediar indiscriminadamente resúmenes incompatibles

names(pm25_diario)

table(pm25_diario$POC, pm25_diario$Sample.Duration, pm25_diario$Method.Code)

table(pm25_diario$POC, pm25_diario$Sample.Duration)
table(pm25_diario$POC, pm25_diario$Method.Code)
table(pm25_diario$Sample.Duration, pm25_diario$Method.Code)


table(pm25_diario$POC)
table(pm25_diario$Sample.Duration)
table(pm25_diario$Method.Code)

# Todas las estaciones cuentan con la misma ventana de tiempo
dias_totales <- as.numeric(max(pm25_diario$Date.Local) - min(pm25_diario$Date.Local)) + 1

info_estacion <- pm25_diario %>%
  group_by(Station) %>%
  summarise(min = min(Date.Local),
            max = max(Date.Local),
            total_dias = n_distinct(Date.Local),
            NAS = dias_totales - total_dias)

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

# Cuenta las apariciones de cada combinación exacta en cada estación
resumen_combinaciones <- pm25_diario %>%
  count(Station, POC, Sample.Duration, Method.Code, name = "frecuencia") 


mejor_combinacion <- pm25_diario %>%
  # 1. Agrupar por la combinación exacta
  group_by(Station, POC, Sample.Duration, Method.Code) %>%
  # 2. Calcular cuántos días operó y su porcentaje promedio de calidad diaria
  summarise(
    dias_observados = n(),
    calidad_promedio = mean(Observation.Percent),
    .groups = "drop_last" # Quita los instrumentos, queda agrupado solo por Station
  ) %>%
  # 3. Ordenar para que la "mejor" quede en la primera fila. 
  # Aquí priorizamos cantidad calidad de observación por encima de los días observados.
  arrange(Station, desc(calidad_promedio), desc(dias_observados))

unoonce <- pm25_diario %>%
  filter(
    Station == "1-11"
  ) %>%
  select(Date.Local, Sample.Duration, Arithmetic.Mean) %>%
  pivot_wider(
    names_from = Sample.Duration,
    values_from = Arithmetic.Mean) 

unoonce <- unoonce %>%
  summarise(dif = '1 HOUR' - '24-HR BLK AVG')
