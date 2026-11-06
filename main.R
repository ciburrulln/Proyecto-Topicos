if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(here, tidyverse, lubridate)
library(dplyr)

# Verificar directorio raíz del proyecto
here()

# Cargar bases de datos
pm25_diario <- read.csv(here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here("datos", "diccionario.csv"))

sitios <- read.csv(here("datos", "sitios.csv"))

tabla <- lapply(pm25_diario, table)

# Variables constantes
sapply(pm25_diario, function(x) length(unique(x)) == 1)

# Eliminar las variables redundantes (Todas las mediciones son en California)
pm25_diario <- pm25_diario %>%
  select(-State.Code, -Parameter.Code, -Parameter.Name, -Units.of.Measure,
         -State.Name)

# Eliminar variables con fuga de información, en este caso eliminamos 
# las variables que registran el valor máximo durante ese día,
# la hora local a la que ocurrió y el índice de calidad de aire
pm25_diario <- pm25_diario %>%
  select(-X1st.Max.Value, -X1st.Max.Hour, -AQI)

