library(readxl)
library(dplyr)
library(openxlsx)

# Ruta del archivo
archivo <- "data/Viajes origen destino optativa Exactas.xlsx"

# Leer las hojas
hoja1 <- read_excel(archivo, sheet = 1)
hoja2 <- read_excel(archivo, sheet = 2)

# purgar formato de las columnas con horas
hoja2$HoraInicio <- format(hoja2$HoraInicio, "%H:%M")
hoja2$HoraFin <- format(hoja2$HoraFin, "%H:%M")

# Índice reiniciado por Identificación
hoja1 <- hoja1 %>%
  group_by(Identificación) %>%
  mutate(occ = row_number()) %>%
  ungroup()

hoja2 <- hoja2 %>%
  group_by(Identificación) %>%
  mutate(occ = row_number()) %>%
  ungroup()

# Unión y limpieza
resultado <- hoja2 %>%
  left_join(hoja1 %>% select(Identificación, occ, Día), by = c("Identificación", "occ")) %>%
  select(-occ)

# Guardar en CSV
write.csv(resultado, "data/base_para_el_practico.csv", row.names = FALSE)

data <- read.csv("data/base_para_el_practico.csv", header = TRUE, sep = ",")
