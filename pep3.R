# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara

# Los datos fueron recolectados en terreno por los estudiantes
# que realizan el analisis. 
# Se basa en una encuesta a las personas que asistieron a ver 
# el eclipse total sol de la Region de Coquimbo en Chile durante
# el 2 de Julio de 2019, se estudian las siguientes variables:

# Procedencia: Regional, Metropolitana o Extranjera
# Continente: El continente de donde viene 
# Origen: Lugar de origen (Pais, Ciudad, Region)
# Nacionalidad: La nacionalidad del encuestado
# Edad
# Noches: Noches de estadia en la region
# Sexo
# Transporte: Transporte que utiliza para llegar a la region
# Hospedaje: 


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

hist(datos.todos$Edad)

hist(datos.todos$Presupuesto)

hist(datos.todos$Noches)

# Dias de nacimiento

# Matias Paredes: d1 = 10
# Jorge Plaza: d2 = 13
# Vicente Rivera: d3 = 10
# Nicolas Alarcon: d4 = 6

# Semilla: d1 * d2 + d3 * d4 = 10 * 13 + 10 * 6 = 190



