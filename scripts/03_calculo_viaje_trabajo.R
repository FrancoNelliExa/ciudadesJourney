library(dplyr)
library(lubridate)

#agregar columna duracion_transporte_min

data <- data %>%
  mutate(
    hora_inicio = hm(hora_inicio),
    hora_fin = hm(hora_fin),
    duracion_transporte_min = as.numeric(hora_fin - hora_inicio, units = "mins"),
    duracion_transporte_min = ifelse(duracion_transporte_min < 0, duracion_transporte_min + 1440, duracion_transporte_min)
  )

#generar csv con tiempo de viaje al trabajo, si no hay datos para una persona se promedia

# Calcular viaje al trabajo por persona_id
tiempos_por_journey <- data %>%
  filter(journey_type == "Type 3: simple commute") %>%
  group_by(persona_id, journey_id) %>%
  summarise(tiempo_total_dia = sum(duracion_transporte_min, na.rm = TRUE), .groups = "drop")

# Calcular promedio de esos viajes por persona
promedios_tipo3 <- tiempos_por_journey %>%
  group_by(persona_id) %>%
  summarise(tiempo_promedio_min = mean(tiempo_total_dia, na.rm = TRUE), .groups = "drop")

# Unir con todas las personas (los que no tienen Type 3 = 0)
tabla_resultado <- data %>%
  distinct(persona_id) %>%
  left_join(promedios_tipo3, by = "persona_id") %>%
  mutate(tiempo_promedio_min = ifelse(is.na(tiempo_promedio_min), 0, tiempo_promedio_min))

# Calcular promedio general de los que no son 0
promedio_general <- mean(tabla_resultado$tiempo_promedio_min[tabla_resultado$tiempo_promedio_min > 0])

# Reemplazar los 0 por el promedio general
tabla_resultado <- tabla_resultado %>%
  mutate(tiempo_promedio_min = ifelse(tiempo_promedio_min == 0, promedio_general, tiempo_promedio_min))

#redondeo
tabla_resultado <- tabla_resultado %>%
  mutate(tiempo_promedio_min = round(tiempo_promedio_min, 0))

write.csv(tabla_resultado, "data/tiempo_vaiaje_trabajo.csv", row.names = FALSE)