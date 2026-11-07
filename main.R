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

# Variables constantes
sapply(pm25_diario, function(x) length(unique(x)) == 1)

# Eliminar las variables redundantes pues todas las mediciones son en California
# y solo estamos calculando pm25. Además Date of Last Change no entrega
# información relevante
pm25_diario <- pm25_diario %>%
  select(-State.Code, -State.Name, 
         -Parameter.Code, -Parameter.Name, -Units.of.Measure,
         -Date.of.Last.Change)

# Eliminar variables con fuga de información, en este caso eliminamos 
# las variables que registran el valor máximo durante ese día,
# la hora local a la que ocurrió y el índice de calidad de aire
pm25_diario <- pm25_diario %>%
  select(-X1st.Max.Value, -X1st.Max.Hour, -AQI)

# Información que ya existe en sitios.cvs y la cruzaremos posteriormente
pm25_diario <- pm25_diario %>%
  select(-Latitude, -Longitude, -Datum, -Local.Site.Name, -Address, 
         -County.Name, -City.Name, -CBSA.Name)

# La exigencia de elegir una combinación coherente recae sobre el 
# instrumento (POC), la duración y el estándar de la norma, 
# no sobre el nombre comercial del método.
# LA IA ME DIJO QUE BORRARA ESTO! PERO NO QUIERO
# pm25_diario <- pm25_diario %>%
  # select(-Method.Code, -Method.Name)

# POC, Sample Duration, Pollutant Standard se eliminarán posteriormente

names(pm25_diario)

# County.Code y Site.Num son las variables que identifican la estación,
# no se deben usar como predictor

#### LIMPIEZA DE OBSERVACIONES ####

# Ver las combinaciones más comunes de instrumento, duración y estándar
# Preocuparse de no promediar indiscriminadamente resúmenes incompatibles

names(pm25_diario)

table(pm25_diario$POC, pm25_diario$Sample.Duration, pm25_diario$Pollutant.Standard)

table(pm25_diario$POC, pm25_diario$Sample.Duration)
table(pm25_diario$POC, pm25_diario$Pollutant.Standard)
table(pm25_diario$Sample.Duration, pm25_diario$Pollutant.Standard)


table(pm25_diario$POC)
table(pm25_diario$Sample.Duration)
table(pm25_diario$Pollutant.Standard)

# County.Code con Site.Num son el id y el date local da justo la observación de 
# una fecha específica en una estación

duplicados <- pm25_diario %>%
  group_by(County.Code, Site.Num, Date.Local) %>% 
  summarise(
    n = n(),
    n_poc = n_distinct(POC),
    n_duracion = n_distinct(Sample.Duration),
    .groups = "drop"
  ) %>%
  filter(n > 1)

table(duplicados$n)
# Todas las estaciones tienen más de un registro al día

resumen_poc <- pm25_diario %>%
  group_by(
    County.Code, Site.Num,
    POC, Sample.Duration
  ) %>%
  summarise(
    n_dias = n_distinct(Date.Local),
    .groups = "drop"
  ) %>%
  arrange(County.Code, Site.Num, desc(n_dias))

pm25_diario %>%
  group_by(County.Code, Site.Num) %>%
  summarise(
    POC_disponibles = paste(sort(unique(POC)), collapse = ", "),
    n_poc = n_distinct(POC),
    .groups = "drop"
  ) %>%
  View()

