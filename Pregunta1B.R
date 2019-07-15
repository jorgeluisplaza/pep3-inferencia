# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara
# Integrantes: 
          # Fernanda Muñoz
          # Jorge Plaza
          # Felipe Vasquez
          # Nicolas Gutierrez

# Dias de nacimiento

# Fernanda Muñoz: d1 = 4
# Jorge Plaza: d2 = 13
# Felipe Vasquez: d3 = 17
# Nicolas Gutierrez: d4 = 17

# Pregunta 1

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

  #Pregunta 1
  #El equipo debe plantear una pregunta de investigaciÃ³n interesante que requiera la comparaciÃ³n de dos
  #proporciones.
  #Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
  #ha de determinar el tamaÃ±o de muestra necesitado para realizar esta comparaciÃ³n estadÃ�stica utilizando
  #muestreo sistemÃ¡tico.
  #Usando la semilla d1Â·d2+ d3Â·d4, el equipo debe realizar este muestreo en los datos de Coquimbo.

  #El equipo debe usar bootstrapping para responder su pregunta de investigaciÃ³n con la muestra obtenida
  
  # Pregunta de investigaciÃ³n: El martes 2 de julio del 2019 a las 15:23 horas la luna comenzÃ³ a tapar el sol.  
  # Ãste fenÃ³meno se dio en una radio de 170 kilÃ³metros de territorio,que se extendiÃ³ desde Guanaqueros a Domeyko.
  # El punto de mayor atracciÃ³n se dio a las 16:38 minutos, donde se produjo la total oscuridad. 
  # La duraciÃ³n de oscuridad va a variar dependiendo de la ubicaciÃ³n de observaciÃ³n.

  # Como se menciona, en la actividad en la Region de Coquimbo se recabaron datos respecto a la procedencia
  # y el presupuesto aproximado que los turistas van a gastar en su estadia en la region
  # Mientras se realizaba el trabajo de terreno, los encuestadores vieron una clara diferencia de gastos entre los 
  # turistas extranjeros y Chilenos proporcionalmente, donde se realiza el supuesto que la proporcion de gastos de los extranjeros
  # era mayor al de los chilenos.
  # Por lo tanto, se plantea la pregunta ¿ La proporcion de Chilenos y Extranjeros con respecto al presupuesto por dia
  # de su estadia en la Region para ver el eclipse total de sol es realmente distinta ?


# Se calcula la semilla con los dias de nacimientos de los integrantes mencionados anteriormente
# Semilla: d1 * d2 + d3 * d4 = 4 * 13 + 17 * 17 = 341
set.seed(341)

# Se obtienen la procedencia y el presupuesto de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Presupuesto=datos.todos$Presupuesto)

# Se utiliza muestreo sistematico para el calculo de la muestra
n.sys <- 60
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Presupuesto, data = muestra, FUN = length)

# Se calculan la cantidad de personas con sus respectivos presupuestos
presupuesto.Menor25.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.Menor25.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Extranjero")), "frec"])
presupuesto.Entre2550.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.Entre2550.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Extranjero")), "frec"])
presupuesto.Entre5075.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.Entre5075.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Extranjero")), "frec"])
presupuesto.Mayor75.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Chileno")), "frec"])
presupuesto.Mayor75.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Extranjero")), "frec"])

# Se agregan los presupuestos de Chilenos y Extranjeros
presupuesto.Menor25 <- c(presupuesto.Menor25.Chilenos, presupuesto.Menor25.Extranjeros)
presupuesto.Entre2550 <- c(presupuesto.Entre2550.Chilenos, presupuesto.Entre2550.Extranjeros)
presupuesto.Entre5075 <- c(presupuesto.Entre5075.Chilenos, presupuesto.Entre5075.Extranjeros)
presupuesto.Mayor75 <- c(presupuesto.Mayor75.Chilenos, presupuesto.Mayor75.Extranjeros)

# Se guarda en un data frame
table.p1.1 <- data.frame(
  "[0, 25000]"=presupuesto.Menor25, 
  "[25000, 50000]"=presupuesto.Entre2550, 
  "[50000, 75000]"=presupuesto.Entre5075,
  "> 75000"=presupuesto.Mayor75
)

# Se definen el nombre de las filas 
rownames(table.p1.1) <- c("Chileno", "Extranjero")

# Se define estadistico para el bootstraping
# Para cada iteracion se hace calculo de tabla
# y de la prueba Chi - Squared
# Se devuelve el estadistico obtenido

foo <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1.1 <- aggregate(frec ~ Procedencia + Presupuesto, data = data, FUN = length)
  presupuesto.Menor25.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.Menor25.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto < 25000 & p1.1$Procedencia == "Extranjero")), "frec"])
  presupuesto.Entre2550.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.Entre2550.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 25000 & p1.1$Presupuesto < 50000 & p1.1$Procedencia == "Extranjero")), "frec"])
  presupuesto.Entre5075.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.Entre5075.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 50000 & p1.1$Presupuesto < 75000 & p1.1$Procedencia == "Extranjero")), "frec"])
  presupuesto.Mayor75.Chilenos <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Chileno")), "frec"])
  presupuesto.Mayor75.Extranjeros <- sum(p1.1[c(which(p1.1$Presupuesto > 75000 & p1.1$Procedencia == "Extranjero")), "frec"])
  
  presupuesto.Menor25 <- c(presupuesto.Menor25.Chilenos, presupuesto.Menor25.Extranjeros)
  presupuesto.Entre2550 <- c(presupuesto.Entre2550.Chilenos, presupuesto.Entre2550.Extranjeros)
  presupuesto.Entre5075 <- c(presupuesto.Entre5075.Chilenos, presupuesto.Entre5075.Extranjeros)
  presupuesto.Mayor75 <- c(presupuesto.Mayor75.Chilenos, presupuesto.Mayor75.Extranjeros)
  
  # Se guarda en un data.frame
  table.p1.1 <- data.frame(
    "[0, 25000]"=presupuesto.Menor25, 
    "[25000, 50000]"=presupuesto.Entre2550, 
    "[50000, 75000]"=presupuesto.Entre5075,
    "> 75000"=presupuesto.Mayor75
  )
  
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

# Se obtiene el valor observado
observado <- chisq.test(table.p1.1)$statistic

# Se calcula el p-valor
count2 <- sum(distribucion > observado)
p.value <- (count2 + 1)/(n.perm + 1)

# Se obtiene el intervalo de confianza
p.95 <- (1 - alpha)*n.perm
distribucion <- sort(distribucion)
limit <- distribucion[p.95]

# Se grafica y se muestran los valores observados y obtenidos
hist(distribucion, breaks = 25)
abline(v=observado, col="blue")
abline(v=limit, col="red")