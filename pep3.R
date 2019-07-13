# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara

library(ggpubr)
library(ggplot2)

# Indicar directorio del archivo .csv
dir <- "/Users/jorgeplaza/Documents/Usach/Inferencia/Pep3"

# Indicar nombre del archivo
basename <- "eclipse2019.csv"

# Se lee el archivo   
file <- file.path(dir, basename)

# Se guardan los datos en una tabla
datos.todos <- read.csv (
  file = file,
  sep = ","
)

hist(datos.todos$Edad)

hist(datos.todos$Presupuesto)

