# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara


library(ggpubr)
library(ggplot2)
library(knitr)
library(tidyr)

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

datos.long <- gather(
  data = datos.todos
)

hist(datos.todos$Edad)

hist(datos.todos$Presupuesto)

hist(datos.todos$Noches)

# Dias de nacimiento

# Matias Paredes: d1 = 10
# Jorge Plaza: d2 = 13
# Vicente Rivera: d3 = 10
# Nicolas Alarcon: d4 = 6

# Semilla: d1 * d2 + d3 * d4 = 10 * 13 + 10 * 6 = 190



