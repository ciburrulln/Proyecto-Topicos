if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(here)



pacman::p_load(here)

# Verificar directorio raíz del proyecto
here()

# Cargar bases de datos
pm25_diario <- read.csv(here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here("datos", "diccionario.csv"))

sitios <- read.csv(here("datos", "sitios.csv"))