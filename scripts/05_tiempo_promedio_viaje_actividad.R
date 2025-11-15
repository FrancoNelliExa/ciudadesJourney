#calculo del tiempo de viaje para todas las actividades

library(dplyr)

assign_travel_time <- function(data, csv_path = "data/tiempo_vaiaje_trabajo.csv") {
  
  # Cargar CSV de tiempos viaje al trabajao
  promedios_tipo3 <- read.csv(csv_path)
  promedios_tipo3$persona_id <- as.character(promedios_tipo3$persona_id)
  
  data <- data %>%
    # Unir tiempos promedio
    left_join(promedios_tipo3, by = "persona_id") %>%
    # Calcular totales por journey
    group_by(persona_id, journey_id) %>%
    mutate(
      total_transporte_journey = sum(duracion_transporte_min, na.rm = TRUE),
      total_actividad_journey  = sum(ifelse(is.na(tiempo_actividad_min), 0, tiempo_actividad_min), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Asignar tiempo de viaje según tipo
    mutate(
      tiempo_viaje_asignado_min = case_when(
        # Type 1: suma total del viaje
        journey_type == "Type 1: single visit" ~ total_transporte_journey,
        
        # Type 2: proporcional al tiempo de actividad
        journey_type == "Type 2: multi-visit" & 
          total_actividad_journey > 0 &
          !(motivo %in% c("Al Trabajo", "Volver a Casa")) ~
          (total_transporte_journey) * (tiempo_actividad_min / total_actividad_journey),
        
        # Type 3: valor directo del CSV
        journey_type == "Type 3: simple commute" ~ tiempo_promedio_min,
        
        # Type 4: como Type 1 pero restando CSV
        journey_type == "Type 4: 1 extra activity" ~ total_transporte_journey - tiempo_promedio_min,
        
        # Type 5: como Type 2 pero restando CSV
        journey_type == "Type 5: >1 extra activity" & 
          total_actividad_journey > 0 &
          !(motivo %in% c("Al Trabajo", "Volver a Casa")) ~
          (total_transporte_journey - tiempo_promedio_min) * (tiempo_actividad_min / total_actividad_journey),
        
        TRUE ~ NA_real_
      )
    ) %>%
    # Redondeo final
    mutate(tiempo_viaje_asignado_min = round(tiempo_viaje_asignado_min, 0))
  
  return(data)
}

data <- assign_travel_time(data)

# Eliminar filas donde tiempo_viaje_asignado_min < 0
data <- data %>%
  mutate(
    tiempo_viaje_asignado_min = as.numeric(tiempo_viaje_asignado_min)
  ) %>%
  filter(tiempo_viaje_asignado_min >= 0 | is.na(tiempo_viaje_asignado_min))


# Calcular promedios por motivo
promedios <- data %>%
  group_by(motivo) %>%
  summarise(
    promedio_tiempo = mean(tiempo_viaje_asignado_min, na.rm = TRUE)
  )

#redondeo
promedios <- promedios %>%
  mutate(promedio_tiempo = round(promedio_tiempo, 0))


write.csv(promedios, "outputs/promedios_viaje_actividad.csv", row.names = FALSE)