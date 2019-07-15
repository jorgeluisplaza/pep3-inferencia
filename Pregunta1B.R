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

#El equipo debe plantear una pregunta de investigación interesante que requiera la comparación de dos
#proporciones.
#Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
#ha de determinar el tamaño de muestra necesitado para realizar esta comparación estad�stica utilizando
#muestreo sistemático.
#Usando la semilla d1·d2+ d3·d4, el equipo debe realizar este muestreo en los datos de Coquimbo.
#El equipo debe usar bootstrapping para responder su pregunta de investigación con la muestra obtenida





# Pregunta de investigación: El martes 2 de julio del 2019  Según cifras oficiales 
#(http://www.diarioeldia.cl/economia/turismo/eclipse-turistas-se-quedaron-en-promedio-3-dias-gastaron-99-mil-diarios)
# llegaron 307.000. Visitantes a la región de coquimbo  a contemplar el eclipse solar Total.
# 15 Mil de estos llegaron en avión. 30.0000 en bus y 262.500 en peajes. Bajo este contexto, el equipo de trabajo realizó diferentes 
# Consultas a extranjeros y nacionales, siendo en particular una de estas el  presupuesto diario. Una vez realizadas las encuestas 
# al compartir la experiencia la mayor�a aseguraba que los extranjeros ten�an mayor presupuesto que los chiles.
# Junto con esto el análisis oficial entrego que los turistas extranjeros gastaron mucho más que los nacionales.
#  en función de comparar con las cifras oficiales y la percepción de los encuestadores se plantea la pregunta de investigación 
# ¿ La porción de chilenos y extranjeros con respecto a su presupuesto 
# diario fue diferente?.



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
#Indicar nombre del archivo
basename <- "eclipse2019.csv"
## Link de descarga: https://docs.google.com/spreadsheets/d/15mG2ATv2ArBVioZ8NSSuixG7Fr_cX8iw_0K3thaGz_M/edit?usp=sharing #
# Acceso correo usach # 

# Se lee el archivo   
file <- file.path(basename)

# Se guardan los datos en una tabla
datos.todos <- read.csv (
  file = file,
  sep = ",",
  encoding = "UTF-8"
)


# Se obtienen la procedencia y el presupuesto de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Presupuesto=datos.todos$Presupuesto)

# Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
# ha de determinar el tamaño de muestra necesitado para realizar esta comparación estad�stica utilizando
#�muestreo sistemático.

# Al utilizar Muestreo Sistematico  debemos utilizar la siguiente formula para calcular el numero de muestras:
P= 0.5 # = Probabilidad de Ocurrencia del Feno�meno Estudiado 
Q = 0.5 # = Probabilidad de que no Ocurra el Feno�meno (q = 1 � p)
N = 86 # numero de la población 
e= 0.05 # Maximo error permitido 5% es un numero alto debido a que se tiene poca cantidad de población.
D = (e^2)/4
nMuestras = (N*P*Q)/((N-1)*D + P*Q)
nMuestras <- ceiling(nMuestras) # aproximar hacia arriba
cat("Numero de muestras a utilizar :")
cat(nMuestras)

# Formula obtenida:  VI Muestreo Sistemático, Dr. Jesús Mellado Bosque
# http://www.uaaan.mx/~jmelbos/muestreo/muapu4.pdf


# Se calcula la semilla con los dias de nacimientos de los integrantes mencionados anteriormente
# Semilla: d1 * d2 + d3 * d4 = 4 * 13 + 17 * 17 = 341
set.seed(341)


### El muestre sistematico es una tecnica de muestreo aleatorio sencillo y eficaz
### La aplicacion del muestreo sistematico se hace de la siguiente forma:

### 1 - Se divide el marco muestral en k = N/n fragmentos, donde N corresponde al tama�o de la poblcacion
### y n al tama�o de la muestra que deseamos

### 2 - Se elige un numero aleatorio de inicio dentro del intervalo escogido

### 3 - Se va eligiendo los individuos de acuerdo al numero de inicio mas el numero k obtenido como una secuencia
### aritmetica simple

### Para la utilizacion del muestreo sistematico se utiliza una libreria externa llamada SamplingUnit, la funcion utilizada
### es sys.sample

# Se utiliza muestreo sistematico para el calculo de la muestra
n.sys <- nMuestras
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Presupuesto, data = muestra, FUN = length)


##  Los presupuestos son presupuestos 
# 1 - [0-25.000]
# 2 - [2500-50.000]
# 3 - [50.000-75.000]
# 4 - [75.000-o Mas]

# Se  calculan la cantidad de personas con sus respectivos presupuestos
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


#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######

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


#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######




# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
bootobj <- boot(muestra, foo, R = n.perm)

distribucion <- bootobj$t



#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######


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