"
Los cuatro problemas principales de la base y su
razón de ser son los siguientes:

1. Duplicidad de filas por estación en un mismo día

El problema: Una misma estación puede registrar
múltiples filas para la misma fecha calendario
[1, 3]. Esto ocurre porque coexisten diferentes
códigos de instrumento (POC), distintas duraciones
de muestra (Sample Duration), diversos estándares
(Pollutant Standard) o tipos de evento (Event Type)
[3].

Consecuencia: Si se promedian las filas a ciegas
o no se selecciona un instrumento/estándar único,
se distorsionan las concentraciones diarias [3].

2. Discontinuidad en las fechas (Gaps temporales)

El problema: La cobertura de fechas es irregular
[1]. Muchas estaciones monitorean cada 3 o 6 días,
por lo que hay días faltantes en la secuencia [3].

Consecuencia: Un día sin registro no significa aire
limpio [3]. Si calculas rezagos tomando simplemente
<<la fila anterior>> del dataset en lugar de la fecha
calendario real (día anterior en el calendario),
introducirás un error en el modelo [3].

3. Fuga de información (Data Leakage)

El problema: El archivo original contiene variables
como AQI, 1st Max Value y 1st Max Hour que se
derivan del propio día objetivo o utilizan
información diaria consolidada [4, 6].

Consecuencia: Deben excluirse (tal como comienza
a hacerlo el script inicial) [6] porque no estarían
disponibles al momento de emitir una predicción
para el día siguiente $t+1$ [2, 4].

4. Identificación compuesta de las estaciones

El problema: Un sitio de monitoreo no se define
por una sola variable, sino por la combinación de
State Code, County Code y Site Num [3, 6].

Consecuencia: El cruce con la tabla sitios.csv para
agregar covariables del sitio (como elevación o
tipo de terreno) debe realizarse sobre esta clave
compuesta [3, 6].

comentario: aqui no se usa esta primera variable que nombra porque era constante.
usamos
County.Code
Date.Local
"

library(dplyr)

# 1. Contar cuántos días tienen registros duplicados
# en una misma estación
head(
  pm25_diario %>%
    count(County.Code, Site.Num, Date.Local) %>%
    filter(n > 1)
)



# 2. Ver las combinaciones de instrumentos (POC)
# y duraciones de muestra

pm25_diario %>%
  count(POC, Sample.Duration, Pollutant.Standard)


# 3. Verificar el número de fechas medidas reales por estación

pm25_diario %>%
  group_by(County.Code, Site.Num) %>%
  summarise(
    dias_medidos = n_distinct(Date.Local),
    fecha_inicio = min(Date.Local),
    fecha_fin = max(Date.Local)
  )

"
Interpretación de tus resultados

1. Duplicados por estación-día (County 1, Site 9):

Confirma que para una misma fecha calendario hay
2 filas por estación.

2. Causa de los duplicados (POC y Sample.Duration):

- Conviven mediciones horarias (1 HOUR) con
  mediciones diarias promedio (24 HOUR y
  24-HR BLK AVG).

- Las mediciones de 1 HOUR no están asociadas
  al estándar de calidad ambiental
  (Pollutant.Standard está vacío), mientras que
  las diarias responden a PM25 24-hour 2012.

- Solución: Debemos filtrar únicamente las
  mediciones consolidadas diarias (24 HOUR y
  24-HR BLK AVG) bajo el estándar
  PM25 24-hour 2012. Si una estación opera dos
  instrumentos a la vez (por ejemplo, POC 1
  y POC 3), debemos establecer una regla clara
  para seleccionar solo uno (por ejemplo,
  priorizar el de menor POC o el que tenga
  mayor historial).

3. Fechas reales por estación:

- Las 30 estaciones presentan entre 2.141 y
  2.190 días medidos de un total teórico de
  2.192 días en los 6 años (2019-2024).

- La cobertura es altísima (~98-99%), pero
  existen pequeños huecos de días sin registro.

- Solución: Debemos expandir las fechas a una
  grilla continua de 2.192 días por estación.
  De esta forma, al calcular el rezago del día
  anterior, lag() tomará la fecha real en el
  calendario y no una fila contigua que
  corresponda a varios días atrás.
"

library(dplyr)
library(tidyr)
library(lubridate)

# 1. Filtrar resúmenes diarios bajo el estándar oficial

pm25_limpio <- pm25_diario %>%
  filter(
    Sample.Duration %in% c("24 HOUR", "24-HR BLK AVG"),
    Pollutant.Standard == "PM25 24-hour 2012"
  )

# 2. Resolver duplicados de múltiples instrumentos (POC) por estación-día

pm25_estacion_dia <- pm25_limpio %>%
  group_by(County.Code, Site.Num, Date.Local) %>%
  slice_min(POC, n = 1) %>% # Mantiene 1 solo instrumento determinista por día
  ungroup()

# 3. Crear grilla temporal continua de fechas calendario (2019-2024)

pm25_estacion_dia <- pm25_estacion_dia %>%
  mutate(Date.Local = as.Date(Date.Local))

panel_completo <- pm25_estacion_dia %>%
  group_by(County.Code, Site.Num) %>%
  complete(Date.Local = seq.Date(as.Date("2019-01-01"), as.Date("2024-12-31"), by = "day")) %>%
  arrange(Date.Local) %>%
  ungroup()

# 4. Construir la variable respuesta Y (t+1) y rezagos temporales

panel_analitico <- panel_completo %>%
  group_by(County.Code, Site.Num) %>%
  arrange(Date.Local) %>%
  mutate(
    # Variable respuesta: concentración media del día t+1
    target_pm25_t1 = lead(Arithmetic.Mean, 1),
    
    # Concentración en el día t y rezagos anteriores
    pm25_t = Arithmetic.Mean,
    pm25_t_1 = lag(Arithmetic.Mean, 1),
    pm25_t_2 = lag(Arithmetic.Mean, 2),
    
    # Variables de calendario
    mes = month(Date.Local),
    dia_semana = wday(Date.Local, label = TRUE)
  ) %>%
  ungroup()


"
P:
tengo una duda. ¿hacer la particion en este momento no haria que el ultimo
dia prediga justo el primer dia de la validacion lo que las instrucciones
dicen que seria un error?

R:
¡Tienes toda la razón en hacerte esa pregunta!
Es una observación estructural muy aguda.

Si la partición se hace cortando las fechas origen
de forma ingenua, efectivamente se puede generar
una fuga de información (data leakage) hacia el
conjunto de validación [4].

---

1. ¿Por qué ocurre este problema?

Una fila analítica se define con un día origen t,
y su variable respuesta Y_{t+1} es la concentración
del día siguiente (t+1) [2].

- Si tomas como día origen t el 31 de diciembre
  de 2022 e incluyes esa fila en el conjunto de
  entrenamiento (2019–2022), su valor a predecir
  Y_{t+1} corresponde al 1 de enero de 2023 [2].

- Esto significa que el modelo estaría entrenándose
  con observaciones pertenecientes al período de
  reserva/prueba (2023–2024) [4].

---

2. La forma correcta de hacer la partición

Para respetar estrictamente las instrucciones del
trabajo, los conjuntos deben delimitarse según la
fecha de la variable respuesta (t+1) [2, 4]:

- Conjunto de Desarrollo (Entrenamiento: 2019–2022):

  - Días origen t: Desde el 1 de enero de 2019
    hasta el 30 de diciembre de 2022.

  - Respuesta a predecir (t+1): Llega hasta
    el 31 de diciembre de 2022 [2, 4].

- Conjunto de Reserva (Evaluación: 2023–2024):

  - Días origen t: Desde el 31 de diciembre
    de 2022 hasta el 30 de diciembre de 2024.

  - Respuesta a predecir (t+1): Abarca desde
    el 1 de enero de 2023 hasta el 31 de
    diciembre de 2024 [2, 4].

---

3. ¿Cómo predice el modelo el primer día
   de validación?

Para evaluar el 1 de enero de 2023
(primer día del período de reserva):

1. La predicción se emite el 31 de diciembre
   de 2022 (día origen t) [2].

2. Se utilizan los rezagos e historia acumulada
   disponible hasta el 31 de diciembre de 2022
   (X_t, X_{t-1}, ...) como predictores [2, 4].

3. Las instrucciones señalan explícitamente que
   la evaluación se hace bajo el supuesto de
   disponer de las mediciones históricas previas
   acumuladas durante el desarrollo [4].
"

# Filtrar conjuntos cuidando la fecha del target (t+1)

train_set <- panel_analitico %>%
  filter(Date.Local >= as.Date("2019-01-01") & Date.Local <= as.Date("2022-12-30"))

test_set <- panel_analitico %>%
  filter(Date.Local >= as.Date("2022-12-31") & Date.Local <= as.Date("2024-12-30"))