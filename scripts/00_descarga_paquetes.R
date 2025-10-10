paquetes <- c("readxl", "dplyr")

instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if(length(instalar)) install.packages(instalar)

lapply(paquetes, library, character.only = TRUE)