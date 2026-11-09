if(!require("pacman")){install.packages("pacman")}; library(pacman)
pacman::p_load(MASS, here, dplyr, tidyr)

# Verificar directorio raíz del proyecto
here::here()

# Cargar bases de datos
pm25_diario <- read.csv(here::here("datos", "pm25_diario.csv"))

diccionario <- read.csv(here::here("datos", "diccionario.csv"))

sitios <- read.csv(here::here("datos", "sitios.csv"))


# verificacion de NA
# apply(is.na(pm25_diario), 2, sum) 
# Da 0 en todas las covariables




# No. 01. Seleccion de variables

# No. 01.1 Seleccion contextual de variables 
pm25_diario <- pm25_diario %>%
  dplyr::select(
    
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

# Las variables Site.Num - County.Code identifican a la estacion de monitoreo
# No deben utilizarse para predicciones.
# Se unificaran en una sola variable llamada Station

"Site.Num: 1 - County.Code: 9, 11, 12, 15
Site.Num: 13 - County.Code: 2
Site.Num: 27 - County.Code: 1003
Site.Num: 29 - County.Code: 18
Site.Num: 31 - County.Code: 1004
Site.Num: 37 - County.Code: 1103, 4008
Site.Num: 41 - County.Code: 1
Site.Num: 45 - County.Code: 6
Site.Num: 47 - County.Code: 3
Site.Num: 53 - County.Code: 8, 1003
Site.Num: 59 - County.Code: 7
Site.Num: 65 - County.Code: 8001, 8005
Site.Num: 69 - County.Code: 2
Site.Num: 71 - County.Code: 27, 306
Site.Num: 73 - County.Code: 1201
Site.Num: 75 - County.Code: 5
Site.Num: 79 - County.Code: 2004
Site.Num: 85 - County.Code: 5, 6
Site.Num: 95 - County.Code: 4
Site.Num: 111 - County.Code: 7, 1004, 2002"



# No. 01.2 Unificacion de covariables y cambios de formato 
"
Crear la variable Station,
cambiar los formatos de las variables:
Date.Local a formato Fecha
Arithemetic.Mean a formato Float
"
pm25_diario <- pm25_diario %>%
  dplyr::mutate(
    dplyr::across(Date.Local, as.Date),
    Station = paste(County.Code, Site.Num, sep = "-")
  ) %>%
  dplyr::relocate(Station, Method.Code) %>%
  dplyr::select(-County.Code, -Site.Num) %>%
  dplyr::mutate(Arithmetic.Mean = as.numeric(Arithmetic.Mean))







# No. 01. LIMPIEZA DE OBSERVACIONES

# Casi todas las estaciones cuentan con la misma ventana de tiempo

# Mientras a la estación 37-4008 le faltan solo 8 días para completar el año


dias_totales <- as.numeric(
  max(pm25_diario$Date.Local) - min(pm25_diario$Date.Local)
) + 1

info_estacion <- pm25_diario %>%
  dplyr::group_by(Station) %>%
  dplyr::summarise(
    min = min(Date.Local),
    max = max(Date.Local),
    total_dias = dplyr::n_distinct(Date.Local),
    total_observaciones = dplyr::n(),
    NAS = dias_totales - total_dias
  )


estacion473 <- pm25_diario %>%
  dplyr::filter(Station == "47-3")

# La estación 47-3 no tiene días en el 2024, por lo que la borramos por simplicidad
pm25_diario <- pm25_diario %>%
  dplyr::filter(Station != "47-3")



# No reescrito desde aqui




# Todas las estaciones tienen más de una máquina 
# o más de una forma de tomar las muestras, por lo que es importante
# elegir la mejor combinación para cada estación

maquinas_por_estacion <- pm25_diario %>%
  dplyr::group_by(Station) %>%
  dplyr::summarise(
    poc_observados = dplyr::n_distinct(POC),
    dur_observadas = dplyr::n_distinct(Sample.Duration),
    met_observados = dplyr::n_distinct(Method.Code),
    combinaciones_totales = dplyr::n_distinct(paste(POC, Sample.Duration, Method.Code))
  )

#### IDENTIFICAR LAS COMBINACIONES ####

combinaciones <- pm25_diario %>%
  dplyr::group_by(Station, POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    n_registros = dplyr::n(),
    n_fechas = dplyr::n_distinct(Date.Local),
    media_porcentaje_obs = mean(Observation.Percent),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Station, dplyr::desc(n_fechas))

# Vemos que cada estación tiene más de una forma de medir la contaminación, por
# lo que debemos elegir una para que sea comparable nuestra predicción

#### ELEGIR MEJOR COMBINACIÓN ####
# Solo entre los años 2019-2022

# las mediciones de menos de 18 horas no son significativas
pm25_filtrado <- pm25_diario %>%
  dplyr::filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) |
      (Sample.Duration == "24-HR BLK AVG" & Observation.Count == 1))

mejores_combinaciones_filtradas <- pm25_filtrado %>%
  dplyr::group_by(Station, POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    dias_observados = dplyr::n(),
    .groups = "drop_last" # No sé pq pone eso
  ) %>%
  # Ordenar para que la con más cobertura quede en primera fila
  dplyr::arrange(Station, dplyr::desc(dias_observados)) 

mejores_combinaciones <- pm25_diario %>%
  dplyr::group_by(Station, POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    dias_observados = dplyr::n(),
    .groups = "drop_last" # No sé pq pone eso
  ) %>%
  # Ordenar para que la con más cobertura quede en primera fila
  dplyr::arrange(Station, dplyr::desc(dias_observados)) 

# Se observa que los promedios de bloque de 24 horas (24 HRS BLK AVG) coinciden en cantidad
# con los promedios horarios (1 HR), si eliminamos las muestras donde con la
# duración 1HR se usan menos de 18 horas, solo se diferencian por el truncamiento 
# a un decimal aplicado por la EPA.
# Por lo que basta analizar la mejor combinación solo con uno de los dos.
# Nos quedamos con 1HR con las observaciones con más de 18 horas para calcular el promedio
# dado que conserva más decimales en Arithmetic.Mean

table(pm25_diario$Observation.Count, pm25_diario$Sample.Duration)

pm25_diario <- pm25_diario %>%
  dplyr::filter(
    (Sample.Duration == "1 HOUR" & Observation.Count >= 18) |
      (Sample.Duration == "24 HOUR" & Observation.Count == 1) )

#### ELEGIR MEJOR COMBINACIÓN ####

mejor_combinacion <- pm25_diario %>%
  dplyr::filter(lubridate::year(Date.Local) %in% 2019:2022) %>%
  dplyr::group_by(Station, POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    dias_observados = dplyr::n_distinct(Date.Local),
    .groups = "drop"
  ) %>%
  dplyr::group_by(Station) %>%
  # Priorizar la combinación con más días
  dplyr::arrange(dplyr::desc(dias_observados), .by_group = TRUE) %>%
  # Elegir una combinación por estación
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  # Ordenar las estaciones numéricamente
  dplyr::arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station))
  )

#### ELIMINAR OBSERVACIONES DUPLICADAS USANDO LA MEJOR COMBINACIÓN ####

pm25_final <- pm25_diario %>%
  dplyr::semi_join(
    mejor_combinacion,
    by = c("Station", "POC", "Sample.Duration", "Method.Code")
  ) %>%
  dplyr::arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station)),
    Date.Local
  )

# Verificar que no existan duplicados
pm25_final %>%
  dplyr::count(Station, Date.Local) %>%
  dplyr::filter(n > 1)

# Analizamos nuevamente la info de cada estacion 
info_estacion_final <- pm25_final %>%
  dplyr::group_by(Station) %>%
  dplyr::summarise(min = min(Date.Local),
                   max = max(Date.Local),
                   total_dias = dplyr::n_distinct(Date.Local),
                   total_observaciones = dplyr::n(),
                   NAS = dias_totales - total_dias)

#### Revisar estación 31-1004 ####

pm25_diario %>%
  dplyr::filter(Station == "31-1004") %>%
  dplyr::group_by(POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = dplyr::n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de Método el 2020!

# Recuperar las observaciones del método anterior
observaciones_recuperadas <- pm25_diario %>%
  dplyr::filter(
    Station == "31-1004",
    POC == 3,
    Sample.Duration == "1 HOUR",
    Method.Code == 204
  ) %>%
  dplyr::anti_join(
    pm25_final %>%
      dplyr::distinct(Station, Date.Local),
    by = c("Station", "Date.Local")
  )

# Incorporarlas a la base final
pm25_final <- dplyr::bind_rows(
  pm25_final,
  observaciones_recuperadas
) %>%
  dplyr::arrange(
    as.integer(sub("-.*", "", Station)),
    as.integer(sub(".*-", "", Station)),
    Date.Local
  )

# Verificar que no existan duplicados
pm25_final %>%
  dplyr::count(Station, Date.Local) %>%
  dplyr::filter(n > 1)


# Analizamos nuevamente la info de cada estacion 
info_estacion_final <- pm25_final %>%
  dplyr::group_by(Station) %>%
  dplyr::summarise(min = min(Date.Local),
                   max = max(Date.Local),
                   total_dias = dplyr::n_distinct(Date.Local),
                   total_observaciones = dplyr::n(),
                   NAS = dias_totales - total_dias)


#### Revisar estación 65-8001 ####

pm25_diario %>%
  dplyr::filter(Station == "65-8001") %>%
  dplyr::group_by(POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = dplyr::n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de POC el 2022 para el método 1 HR (yo lo cambiaría)

#### Revisar estación 71-27 ####

pm25_diario %>%
  dplyr::filter(Station == "71-27") %>%
  dplyr::group_by(POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = dplyr::n_distinct(Date.Local),
    .groups = "drop"
  )
# Hubo cambio de método el 2022 para la duración 1 HR 
# PERO tiene más observaciones la duración 24 HRS
# 2120 > 1909 (CASI 200 OBSERVACIONES MÁS)


#### Revisar estación 37-4008 ####

pm25_diario %>%
  dplyr::filter(Station == "37-4008") %>%
  dplyr::group_by(POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = dplyr::n_distinct(Date.Local),
    .groups = "drop"
  )
# La duración 1 hr llega hasta el 2021, así que nos mantenemos

#### Revisar estación 37-1103 ####

pm25_diario %>%
  dplyr::filter(Station == "37-1103") %>%
  dplyr::group_by(POC, Sample.Duration, Method.Code) %>%
  dplyr::summarise(
    inicio = min(Date.Local),
    fin = max(Date.Local),
    dias = dplyr::n_distinct(Date.Local),
    .groups = "drop"
  )
# La duración 24 hrs tiene más de 1000 observaciones más que 
# la duración 1 hr

pm25_diario <- pm25_final


#### IMPUTAR ####

# Días sin medición -------------------------------------------------------

pm25_diario <- pm25_diario %>%
  dplyr::group_by(Station) %>%
  # Rellena todos los días calendario faltantes entre el inicio y fin de cada estación
  tidyr::complete(Date.Local = seq.Date(min(as.Date("2019-01-01"), na.rm = TRUE), max(as.Date("2024-12-31"), na.rm = TRUE), by = "day")) %>%
  dplyr::ungroup()

apply(is.na(pm25_diario), 2, sum)

# 2. Contar los días sin medición (NA) por estación
conteo_faltantes <- pm25_diario %>%
  dplyr::group_by(Station) %>%
  dplyr::summarise(
    dias_totales       = dplyr::n(),
    dias_con_medicion  = sum(!is.na(Arithmetic.Mean)),
    dias_sin_medicion  = sum(is.na(Arithmetic.Mean)),
    porcentaje_faltante = round((dias_sin_medicion / dias_totales) * 100, 2)
  ) %>%
  dplyr::arrange(dplyr::desc(dias_sin_medicion)) # Ordena de la estación con más baches a la que menos tiene

# Ver la tabla resumen
print(conteo_faltantes)
##son maximo 100 dias sin medición 101/2091 = 0.04607664 -> 4% máximo


#Los días sin datos ahora quedan como NA
pm25_diario <- pm25_diario %>%
  dplyr::group_by(Station) %>%
  tidyr::complete(Date.Local = seq.Date(min(Date.Local, na.rm = TRUE), max(Date.Local, na.rm = TRUE), by = "day")) %>%
  dplyr::arrange(Station, Date.Local) %>%
  #Rezagos
  dplyr::mutate(
    pm25_hoy    = Arithmetic.Mean,
    pm25_lag1   = dplyr::lag(pm25_hoy, 1),   # Estrictamente el día calendario anterior (o NA si faltaba)
    pm25_lag7   = dplyr::lag(pm25_hoy, 7),   # Estrictamente 7 días calendario atrás
    pm25_futuro = dplyr::lead(pm25_hoy, 1)   # Estrictamente el día calendario siguiente
  ) %>%
  dplyr::ungroup()

sum(is.na(pm25_diario$pm25_hoy)) #NAs totales por dia

##NAs por estación (repartidos)
conteo_por_estacion <- pm25_diario %>%
  dplyr::group_by(Station) %>%
  dplyr::summarise(dias_faltantes = sum(is.na(pm25_hoy)))

view(conteo_por_estacion)

##ejemplo en de una estación en específico##############3
pm25_diario %>%
  dplyr::filter(Station == "1-9") %>% # Cambia esto por un ID de tu base
  dplyr::select(Date.Local, pm25_hoy, pm25_lag1, pm25_lag7, pm25_futuro) %>%
  utils::head(15)


##fechas NA
pm25_diario_NA <- pm25_diario %>% dplyr::filter(is.na(pm25_hoy))
pm25_diario_NA

names(pm25_diario)

#PARTICIONES-----------------------------------------------------------------

entrenamiento <- pm25_diario %>% dplyr::filter(Date.Local >= "2019-01-01" & Date.Local <= "2021-12-31")
validacion   <- pm25_diario %>% dplyr::filter(Date.Local >= "2022-01-01" & Date.Local <= "2022-12-31")
reserva    <- pm25_diario %>% dplyr::filter(Date.Local >= "2023-01-01" & Date.Local <= "2024-12-31")

pm25_diario
modelo_lm <- stats::lm(pm25_futuro ~ pm25_hoy + pm25_lag1 + pm25_lag7, data = entrenamiento)
summary(modelo_lm)
names(pm25_diario)

#lm con tipo de evento -> es significativo pero no aporta??
modelo_lm1 <- stats::lm(pm25_futuro ~ pm25_hoy + pm25_lag1 + pm25_lag7 + Event.Type , data = entrenamiento)
summary(modelo_lm1)

#PARA EL PRIMER MODELO
real <- pm25_diario$pm25_futuro
pred <- stats::predict(modelo_lm)

mae  <- function(real, pred) mean(abs(real - pred), na.rm = TRUE)
rmse <- function(real, pred) sqrt(mean((real - pred)^2, na.rm = TRUE))

##TIENEN DISTINTA LONGITUD real y pred -> POR LOS NA
#mae de regresion lineal
mae(real, pred)

#rmse
rmse(real, pred) #no cambia tanto -> no hay errores tan grandes 

##boxcox no se puede pq hay NAs
MASS::boxcox(modelo_lm)



rm(list = setdiff(ls(), c("entrenamiento",
                          "validacion",
                          "reserva",
                          "diccionario", "pm25_final",
                          "sitios",
                          "pm25_diario")))

