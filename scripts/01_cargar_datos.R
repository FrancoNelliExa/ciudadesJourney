library("readxl")

data <- read_excel("data/Viajes origen destino optativa Exactas.xlsx")

head(data)
str(data)
summary(data)