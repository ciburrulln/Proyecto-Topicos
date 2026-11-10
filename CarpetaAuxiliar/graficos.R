if(!require(pacman)){install.packages("pacman")}; library(pacman)

pacman::p_load(
  "ggplot2",
  "dplyr",
  "maps",
  "sf",
  "viridis",
  "here"
)

pm25_diario <- read.csv(here::here("datos", "pm25_diario.csv"))


# 2. PREPARAR LOS DATOS
pm25 <- pm25_diario %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    PM25 = as.numeric(Arithmetic.Mean),
    Fecha = as.Date(Date.Local),
    Anio = as.integer(format(Fecha, "%Y"))
  ) %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(PM25),
    !is.na(Fecha)
  )

# 3. OBTENER MAPA DE CALIFORNIA
california <- map_data("state") %>%
  filter(region == "california")

# 4. CALCULAR PROMEDIO DE PM2.5 POR ESTACIÓN
pm25_estaciones <- pm25 %>%
  group_by(
    Latitude,
    Longitude,
    Local.Site.Name,
    City.Name
  ) %>%
  summarise(
    PM25_promedio = mean(PM25, na.rm = TRUE),
    .groups = "drop"
  )

# 5. MAPA 1: UBICACIÓN DE LAS ESTACIONES
mapa_estaciones <- ggplot() +
  geom_polygon(
    data = california,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray50"
  ) +
  geom_point(
    data = pm25_estaciones,
    aes(x = Longitude, y = Latitude),
    color = "red",
    size = 2.5,
    alpha = 0.8
  ) +
  coord_quickmap() +
  labs(
    title = "Estaciones de monitoreo de PM2.5",
    subtitle = "California, Estados Unidos",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_minimal()

print(mapa_estaciones)

# 6. MAPA 2: CONCENTRACIÓN PROMEDIO DE PM2.5
mapa_concentracion <- ggplot() +
  geom_polygon(
    data = california,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray50"
  ) +
  geom_point(
    data = pm25_estaciones,
    aes(
      x = Longitude,
      y = Latitude,
      color = PM25_promedio,
      size = PM25_promedio
    ),
    alpha = 0.85
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "PM2.5\n(µg/m³)"
  ) +
  labs(
    title = "Concentración promedio de PM2.5",
    subtitle = "Promedio de todas las observaciones por estación",
    x = "Longitud",
    y = "Latitud",
    size = "PM2.5"
  ) +
  coord_quickmap() +
  theme_minimal()

print(mapa_concentracion)

# 7. MAPA 3: CONCENTRACIÓN PROMEDIO POR AÑO
pm25_anual <- pm25 %>%
  group_by(
    Anio,
    Latitude,
    Longitude,
    Local.Site.Name
  ) %>%
  summarise(
    PM25_promedio = mean(PM25, na.rm = TRUE),
    .groups = "drop"
  )

mapa_anual <- ggplot() +
  geom_polygon(
    data = california,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray50"
  ) +
  geom_point(
    data = pm25_anual,
    aes(
      x = Longitude,
      y = Latitude,
      color = PM25_promedio,
      size = PM25_promedio
    ),
    alpha = 0.85
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "PM2.5\n(µg/m³)"
  ) +
  coord_quickmap() +
  facet_wrap(~Anio) +
  labs(
    title = "Distribución espacial de PM2.5 por año",
    subtitle = "Promedio anual por estación de monitoreo",
    x = "Longitud",
    y = "Latitud",
    size = "PM2.5"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(mapa_anual)

# 8. GUARDAR LOS MAPAS EN FORMATO PNG
ggsave(here::here("CarpetaAuxiliar", "mapa_estaciones_PM25.png"),
       mapa_estaciones, width = 9, height = 7, dpi = 300)

ggsave(here::here("CarpetaAuxiliar", "mapa_concentracion_PM25.png"),
       mapa_concentracion, width = 9, height = 7, dpi = 300)

ggsave(here::here("CarpetaAuxiliar", "mapa_anual_PM25.png"),
       mapa_anual, width = 12, height = 9, dpi = 300)

cat("¡Proceso terminado! Se generaron los tres mapas y los archivos CSV.\n")


