library(dplyr)

# Definir categorías de base
base_home <- "Volver a Casa"
base_work <- "Al Trabajo"

# Clasificar viajes
journeys <- data %>%
  arrange(Identificación, HoraInicio) %>%
  group_by(Identificación) %>%
  mutate(
    next_motivo = lead(Motivo.del.Viaje),
    prev_motivo = lag(Motivo.del.Viaje)
  ) %>%
  summarise(
    n_activities = sum(!Motivo.del.Viaje %in% c(base_home, base_work)),
    home_trips = sum(Motivo.del.Viaje == base_home),
    work_trips = sum(Motivo.del.Viaje == base_work),
    tipo_journey = case_when(
      home_trips > 0 & work_trips == 0 & n_activities == 1 ~ "Type 1: One-visit",
      home_trips > 0 & work_trips == 0 & n_activities > 1 ~ "Type 2: Multi-visit",
      home_trips > 0 & work_trips > 0 & n_activities == 0 ~ "Type 3: Simple commute",
      home_trips > 0 & work_trips > 0 & n_activities == 1 ~ "Type 4: One extra activity",
      home_trips > 0 & work_trips > 0 & n_activities > 1 ~ "Type 5: Several activities",
      TRUE ~ "Unclassified"
    )
  )
