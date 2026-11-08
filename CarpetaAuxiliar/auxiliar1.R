if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(here, dplyr, tidyr)

# Verificar directorio raíz del proyecto
here()

# Cargar bases de datos
pm25_diario <- read.csv(here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here("datos", "diccionario.csv"))

sitios <- read.csv(here("datos", "sitios.csv"))

#### LIMPIEZA DE VARIABLES ####

names(pm25_diario)


pm25_diario <- pm25_diario %>%
  select(
    
    # Las siguientes variables son constantes
    -State.Code, # 6
    -State.Name, # California
    -Parameter.Code, # 88101 
    -Parameter.Name, # PM2.5 - Local Conditions
    -Units.of.Measure, # Micrograms/cubic meter (LC)
    
    # La siguiente variable no es relevante para el estudio
    -Date.of.Last.Change, # Ultima fecha de modificación no aporta info
    
    # Las siguientes variables se excluyen en el pdf la las instrucciones
    -X1st.Max.Value, # Medida derivada de la concentración no aporta info 
    -X1st.Max.Hour, # Medida derivada de la concentración no aporta info
    -AQI, # Excluido por contexto
    
    # Información que ya existe en sitios.cvs y la cruzaremos posteriormente
    -Latitude,
    -Longitude,
    -Datum,
    -Local.Site.Name,
    -Address, 
    -County.Name,
    -City.Name,
    -CBSA.Name
  
    )

