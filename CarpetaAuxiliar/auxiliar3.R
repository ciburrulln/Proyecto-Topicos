

"
Para dejar la base de datos completamente lista
para el modelado según lo exigido por las
Instrucciones, nos faltan 4 elementos clave en
la ingeniería de variables y estructuración final
[2]:

---

1. Promedios móviles históricos (7 y 30 días)

Las instrucciones especifican que como predictores
deben incluirse resúmenes de los 7 y 30 días
anteriores a la fecha origen t [2]. Estos capturan
la tendencia reciente y el nivel de fondo de la
contaminación.

2. Antigüedad de la última medición
   (para el Baseline de Persistencia)

Uno de los modelos de referencia exigidos es la
última concentración disponible acompañada de su
antigüedad (cuántos días atrás ocurrió esa
medición) [4]. Si el día t tuvo medición, su
antigüedad es 0; si no hubo medición en t ni en
t-1, pero sí en t-2, la concentración observada
es la de t-2 y su antigüedad es de 2 días.

3. Atributos físicos del sitio (sitios.csv)

Debemos realizar el cruce (left_join) con la tabla
sitios.csv [6] para incorporar las características
estáticas de cada estación: elevación, uso de
suelo (Land Use) y tipo de entorno
(Location Setting) [2].

4. Filtrado de registros evaluables

Debemos asegurar que las filas utilizadas para
entrenamiento y validación tengan una variable
respuesta Y_{t+1} válida (no NA) y que se descarten
las fechas origen que no tengan información
histórica mínima para construir los predictores.

"


library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

# 1. Cargar y filtrar mediciones diarias bajo estándar oficial

pm25_limpio <- pm25_diario %>%
  filter(
    Sample.Duration %in% c("24 HOUR", "24-HR BLK AVG"),
    Pollutant.Standard == "PM25 24-hour 2012"
  ) %>%
  group_by(County.Code, Site.Num, Date.Local) %>%
  slice_min(POC, n = 1) %>% # Resolvemos duplicados de instrumento determinísticamente
  ungroup()

# 2. Cargar datos de sitios y seleccionar atributos

sitios_clean <- sitios %>%
  select(
    County.Code,
    Site.Num,
    Elevation,
    Land.Use,
    Location.Setting,
    Latitude,
    Longitude
  ) %>%
  distinct(County.Code, Site.Num, .keep_all = TRUE)

# 3. Crear grilla continua (2019-2024) y calcular rezagos/promedios

panel_completo <- pm25_limpio %>%
  mutate(Date.Local = as.Date(Date.Local)) %>%
  group_by(County.Code, Site.Num) %>%
  complete(
    Date.Local = seq.Date(
      as.Date("2019-01-01"),
      as.Date("2024-12-31"),
      by = "day"
    )
  ) %>%
  arrange(Date.Local) %>%
  mutate(
    # Variable respuesta: concentración media en t+1
    target_pm25_t1 = lead(Arithmetic.Mean, 1),
    
    # Concentración en t
    pm25_t = Arithmetic.Mean,
    
    # Promedios móviles de 7 y 30 días anteriores (excluyendo el futuro)
    pm25_prom_7d = rollapply(
      Arithmetic.Mean,
      width = 7,
      FUN = mean,
      na.rm = TRUE,
      align = "right",
      fill = NA
    ),
    
    pm25_prom_30d = rollapply(
      Arithmetic.Mean,
      width = 30,
      FUN = mean,
      na.rm = TRUE,
      align = "right",
      fill = NA
    ),
    
    # Baseline de Persistencia y Antigüedad de la medición (días transcurridos)
    pm25_last_obs = na.locf(Arithmetic.Mean, na.rm = FALSE),
    
    pos_obs = ifelse(!is.na(Arithmetic.Mean), row_number(), NA),
    
    pos_obs_locf = na.locf(pos_obs, na.rm = FALSE),
    
    dias_antiguedad = row_number() - pos_obs_locf,
    
    # Calendario
    mes = as.factor(month(Date.Local)),
    dia_semana = as.factor(wday(Date.Local, label = TRUE))
  ) %>%
  ungroup() %>%
  # Unir atributos físicos del sitio
  left_join(sitios_clean, by = c("County.Code", "Site.Num"))

# 4. Partición limpia por fecha del objetivo (t+1)

datos_entrenamiento <- panel_completo %>%
  filter(
    Date.Local >= as.Date("2019-01-01") &
      Date.Local <= as.Date("2022-12-30")
  ) %>%
  filter(!is.na(target_pm25_t1))

datos_prueba <- panel_completo %>%
  filter(
    Date.Local >= as.Date("2022-12-31") &
      Date.Local <= as.Date("2024-12-30")
  ) %>%
  filter(!is.na(target_pm25_t1))

"
Sí, estructuralmente esta ya es la base de datos
analítica final lista para comenzar el modelado [2].

Para tu tranquilidad, el objeto panel_completo
(y sus particiones datos_entrenamiento y
datos_prueba) cumple con los requerimientos
especificados en las instrucciones [2, 4]:

---

Lo que contiene la base lista:

1. Identificador único de fila:

Estación (County.Code, Site.Num) y día origen
Date.Local [2].

2. Variable respuesta (Y):

target_pm25_t1 (media de PM2.5 del día t+1) [2].

3. Predictores de historia y tendencia:

- pm25_t (concentración del día origen).
- pm25_prom_7d y pm25_prom_30d (resúmenes de
  los 7 y 30 días previos) [2].

4. Variables para el Baseline de Persistencia:

- pm25_last_obs (última concentración observada).
- dias_antiguedad (antigüedad en días calendario
  de esa última medición) [4].

5. Covariables de sitio y calendario:

Elevation, Land.Use, Location.Setting,
Latitude, Longitude, mes y dia_semana [2].

6. Sin Fuga de Información:

Separada estrictamente por la fecha de la variable
respuesta para evitar que datos de 2023 se filtren
al entrenamiento [4].

---

Único detalle práctico a tener en cuenta antes
de ajustar modelos:

Los primeros 30 días de enero de 2019 en
datos_entrenamiento tendrán algunos valores NA
en pm25_prom_30d porque no hay registros
anteriores a 2019 para promediar.

Puedes filtrar esos primeros días del conjunto
de entrenamiento con:

filter(!is.na(pm25_prom_30d))

O dejar que el algoritmo de modelado gestione
los missing iniciales.
"
