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
# información relevante. Method.Name da la misma información que Method.Code
pm25_diario <- pm25_diario %>%
  select(-State.Code, -State.Name, 
         -Parameter.Code, -Parameter.Name, -Units.of.Measure,
         -Date.of.Last.Change, -Method.Name)

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
  relocate(Station) %>%
  select(-County.Code, -Site.Num)

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



# Separar la data para decidir qué elegir por estación sin usar la data del 2022
# Solo usaremos 2019, 2020 y 2021

inicio <- as.Date("2019-01-01")
fin <- as.Date("2021-12-31")

total_dias <- as.integer(fin - inicio) + 1
