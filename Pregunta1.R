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
library(boot)
library(tidyr)
library(devtools)
#install_github("DFJL/SamplingUtil")
library(SamplingUtil)

# Indicar directorio del archivo .csv
dir <- ""

# Indicar nombre del archivo
basename <- "eclipse2019.csv"

# Se lee el archivo   
file <- file.path(basename)

# Se guardan los datos en una tabla
datos.todos <- read.csv (
  file = file,
  sep = ",",
  encoding = "UTF-8"
)

# Dias de nacimiento

# Matias Paredes: d1 = 10
# Jorge Plaza: d2 = 13
# Vicente Rivera: d3 = 10
# Nicolas Alarcon: d4 = 6


# ************* PREGUNTA 1 FORMA A *********************** #
# Semilla: d1 * d2 + d3 * d4 = 10 * 13 + 10 * 6 = 190
set.seed(190)

# Se obtienen la procedencia y la localidad de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Localidad=datos.todos$Localidad)

# Se utiliza muestreo sistematico para el calculo de la muestra
n.sys <- 50
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = muestra, FUN = length)

# Se calculan la cantidad de personas en las tres localidades 
vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]

# Se guarda en un data.frame
table.p1.1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)

# Se definen el nombre de las filas 
rownames(table.p1.1) <- c("Chileno", "Extranjero")

# Se define estadistico para el bootstraping
foo <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = data, FUN = length)
  vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
  higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
  serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]
  table.p1.1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)
  rownames(table.p1.1) <- c("Chileno", "Extranjero")
  chi <- chisq.test(table.p1.1)
  return(chi$statistic)
}

# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
bootobj <- boot(muestra, foo, R = n.perm)

distribucion <- bootobj$t

# Se define un alpha de 0.05
alpha <- 0.05
observado <- chisq.test(table.p1.1)$statistic
count <- sum(distribucion > observado)
p.value <- (count + 1)/(n.perm + 1)
p.95 <- (1 - alpha)*n.perm
distribucion <- sort(distribucion)
limit <- distribucion[p.95]

# Se grafica y se mmuestran los valores observados y obtenidos
hist(distribucion, breaks = 25)
abline(v=observado, col="blue")
abline(v=limit, col="red")

# ************* FIN PREGUNTA 1 FORMA A *********************** #

# ************* PREGUNTA 1 FORMA B *********************** #

# Semilla: d1 * d2 + d3 * d4 = 10 * 13 + 10 * 6 = 190
set.seed(190)

# Se obtienen la procedencia y la localidad de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Localidad=datos.todos$Localidad)

# Se utiliza muestreo sistematico para el calculo de la muestra
n.sys <- 50
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = muestra, FUN = length)

# Se calculan la cantidad de personas en las tres localidades 
vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]

# Se guarda en un data.frame
table.p1.1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)

# Se definen el nombre de las filas 
rownames(table.p1.1) <- c("Chileno", "Extranjero")

# Se define estadistico para el bootstraping
foo <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = data, FUN = length)
  vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
  higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
  serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]
  table.p1.1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)
  rownames(table.p1.1) <- c("Chileno", "Extranjero")
  chi <- chisq.test(table.p1.1)
  return(chi$statistic)
}

# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
bootobj <- boot(muestra, foo, R = n.perm)

distribucion <- bootobj$t

# Se define un alpha de 0.05
alpha <- 0.05
observado <- chisq.test(table.p1.1)$statistic
count <- sum(distribucion > observado)
p.value <- (count + 1)/(n.perm + 1)
p.95 <- (1 - alpha)*n.perm
distribucion <- sort(distribucion)
limit <- distribucion[p.95]

# Se grafica y se mmuestran los valores observados y obtenidos
hist(distribucion, breaks = 25)
abline(v=observado, col="blue")
abline(v=limit, col="red")
