if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(MASS, here, dplyr, tidyr)

# Verificar directorio raíz del proyecto
here::here()

# Cargar bases de datos
pm25_diario <- read.csv(here::here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here::here("datos", "diccionario.csv"))

sitios <- read.csv(here::here("datos", "sitios.csv"))

source(here::here("limpieza.R"))

rm(
  observaciones_recuperadas,
  mejor_combinacion,
  info_estacion_final,
  pm25_diario_original,
  dias_totales
)


source(here::here("referencia_bloques.R"))

rm(
  comprobacion,
  resultados_antiguedad,
  resultados_globales
)

source(here::here("referencia_patron.R"))

rm(
  resultados_estacional, 
  comparacion_referencias
)

source(here::here("modelos_pm25.R"))
