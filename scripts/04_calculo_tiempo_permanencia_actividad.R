#tiempo de permanencia en las actividades

library(dplyr)
library(lubridate)

calcular_tiempo_actividad <- function(data) {
  data <- data %>%
    mutate(row_id = row_number()) %>%
    group_by(persona_id, journey_id) %>%
    mutate(
      next_hora_inicio = lead(hora_inicio),
      next_journey_id = lead(journey_id),
      # diferencia directa en minutos
      tiempo_actividad_min = as.numeric(next_hora_inicio - hora_fin, units = "mins"),
      # si pasa medianoche (tiempo negativo), sumar 24h
      tiempo_actividad_min = ifelse(tiempo_actividad_min < 0, tiempo_actividad_min + 1440, tiempo_actividad_min),
      # si cambia de journey o no hay siguiente, poner NA
      tiempo_actividad_min = ifelse(is.na(next_hora_inicio) | next_journey_id != journey_id,
                                    NA,
                                    tiempo_actividad_min),
      # excluir actividades en casa o trabajo
      tiempo_actividad_min = ifelse(motivo %in% c("Al Trabajo", "Volver a Casa"),
                                    NA,
                                    tiempo_actividad_min)
    ) %>%
    ungroup() %>%
    arrange(row_id) %>%
    select(-row_id, -next_hora_inicio, -next_journey_id)
  
  return(data)
}
data <- calcular_tiempo_actividad(data)