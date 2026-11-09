if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(MASS, here, dplyr, tidyr)

# Verificar directorio raíz del proyecto
here::here()

# Cargar bases de datos
pm25_diario <- read.csv(here::here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here::here("datos", "diccionario.csv"))

sitios <- read.csv(here::here("datos", "sitios.csv"))

source(here::here("limpieza.R"))
source(here::here("modelos.R"))
source(here::here("Analisis", "M1_V2.R"))
source(here::here("Analisis", "M2.R"))
# source(here::here("Analisis", "M3.R"))
