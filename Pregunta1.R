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




### Pregunta 1 -> Comparacion de dos proporciones, utilizar bootstrapping y muestreo sistematico

# Como se menciona, en la actividad en la Region de Coquimbo se recabaron datos respecto a la procedencia
# y la localidad de los turistas, de los cuales se tienen estas opciones:

# Procedencia: El turista es Chileno o Extranjero
# Localidad: Lugar en la Region que asiste a ver el eclipse. La Higuera, La Serena o Vicuña

# Con respecto a estos datos, seria interesante conocer si la proporcion de Chilenos y Extranjeros
# es la misma con respecto al lugar que van a asistir. En el trabajo de terreno se tuvo un supuesto con respecto a ello
# donde se piensa que los extranjeros van en su mayoria a la Higuera y los Chilenos a Vicuña y la Serena. Lo que nos hace preguntarnos
# ? La proporcion de la procedencia de los turistas y la localidad a la que asistiran a ver el eclipse es realmente distinta?

# Se hace calculo de la semilla con respecto a los dias de nacimiento de los investigadores mencionados previamente
# Semilla: d1 * d2 + d3 * d4 = 10 * 13 + 10 * 6 = 190
set.seed(190)

# Para realizar el estudio de proporciones, se realiza una tabla donde las filas corresponden a 
# la procedencia del turista y las columnas las localidades

# Cada campo de la tabla corresponde al numero de turistas que asiste a la localidad

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
  Vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
  higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
  serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]
  table.p1.1 <- data.frame(Vicuña=Vicuña, "La Higuera"=higuera, "La Serena"=serena)
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

# Como se menciona, en la actividad en la Region de Coquimbo se recabaron datos respecto a la procedencia
# y el presupuesto aproximado que los turistas van a gastar en su estadia en la region


# Dias de nacimiento

# Fernanda Mu?oz: d1 = 4
# Nicolas Gutierrez: d2 = 17
# Sandra Hernandez: d3 = 26
# Felipe Vasquez: d4 = 17


# Semilla: d1 * d2 + d3 * d4 = 4 * 17 + 26 * 17 = 510
set.seed(510)

# Se obtienen la procedencia y la localidad de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Presupuesto=datos.todos$Presupuesto)

# Se utiliza muestreo sistematico para el calculo de la muestra
n.sys <- 60
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Presupuesto, data = muestra, FUN = length)

# Se calculan la cantidad de personas en las tres localidades 
presupuesto.menor25.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.menor25.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Extranjero")), "frec"])
presupuesto.entre2550.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.entre2550.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Extranjero")), "frec"])
presupuesto.entre5075.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.entre5075.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Extranjero")), "frec"])
presupuesto.mayor75.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.mayor75.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Extranjero")), "frec"])

presupuesto.menor25 <- c(presupuesto.menor25.chilenos, presupuesto.menor25.extranjeros)
presupuesto.entre2550 <- c(presupuesto.entre2550.chilenos, presupuesto.entre2550.extranjeros)
presupuesto.entre5075 <- c(presupuesto.entre5075.chilenos, presupuesto.entre5075.extranjeros)
presupuesto.mayor75 <- c(presupuesto.mayor75.chilenos, presupuesto.mayor75.extranjeros)

# Se guarda en un data.frame
table.p1.1 <- data.frame(
  "[0, 25000]"=presupuesto.menor25, 
  "[25000, 50000]"=presupuesto.entre2550, 
  "[50000, 75000]"=presupuesto.entre5075,
  "> 75000"=presupuesto.mayor75
)

# Se definen el nombre de las filas 
rownames(table.p1.1) <- c("Chileno", "Extranjero")

# Se define estadistico para el bootstraping
foo <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1.1 <- aggregate(frec ~ Procedencia + Presupuesto, data = data, FUN = length)
  presupuesto.menor25.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.menor25.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Extranjero")), "frec"])
  presupuesto.entre2550.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.entre2550.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Extranjero")), "frec"])
  presupuesto.entre5075.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.entre5075.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Extranjero")), "frec"])
  presupuesto.mayor75.chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.mayor75.extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Extranjero")), "frec"])
  
  presupuesto.menor25 <- c(presupuesto.menor25.chilenos, presupuesto.menor25.extranjeros)
  presupuesto.entre2550 <- c(presupuesto.entre2550.chilenos, presupuesto.entre2550.extranjeros)
  presupuesto.entre5075 <- c(presupuesto.entre5075.chilenos, presupuesto.entre5075.extranjeros)
  presupuesto.mayor75 <- c(presupuesto.mayor75.chilenos, presupuesto.mayor75.extranjeros)
  
  # Se guarda en un data.frame
  table.p1.1 <- data.frame(
    "[0, 25000]"=presupuesto.menor25, 
    "[25000, 50000]"=presupuesto.entre2550, 
    "[50000, 75000]"=presupuesto.entre5075,
    "> 75000"=presupuesto.mayor75
  )
  
  rownames(table.p1.1) <- c("Chileno", "Extranjero")
  chi <- chisq.test(table.p1.1)
  return(chi$statistic)
}

# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
bootobj <- boot(muestra, foo, R = n.perm)

distribucionFormaB <- bootobj$t

# Se define un alpha de 0.05
alpha <- 0.05
observado <- chisq.test(table.p1.1)$statistic
count <- sum(distribucionFormaB > observado)
p.value <- (count + 1)/(n.perm + 1)
p.95 <- (1 - alpha)*n.perm
distribucion <- sort(distribucionFormaB)
limit <- distribucion[p.95]

# Se grafica y se mmuestran los valores observados y obtenidos
hist(distribucion, breaks = 25)
abline(v=observado, col="blue")
abline(v=limit, col="red")
