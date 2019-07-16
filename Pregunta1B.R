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
#ha de determinar el tamaño de muestra necesitado para realizar esta comparación estadística utilizando
#muestreo sistemático.
#Usando la semilla d1·d2+ d3·d4, el equipo debe realizar este muestreo en los datos de Coquimbo.
#El equipo debe usar bootstrapping para responder su pregunta de investigación con la muestra obtenida




# Pregunta de investigación: El martes 2 de julio del 2019  Según cifras oficiales 
#(http://www.diarioeldia.cl/economia/turismo/eclipse-turistas-se-quedaron-en-promedio-3-dias-gastaron-99-mil-diarios)
# llegaron 307.000. Visitantes a la región de coquimbo  a contemplar el eclipse solar Total.
# 15 Mil de estos llegaron en avión. 30.0000 en bus y 262.500 en peajes. Bajo este contexto, el equipo de trabajo realizó diferentes 
# Consultas a extranjeros y nacionales, siendo en particular una de estas el  presupuesto diario. Una vez realizadas las encuestas, 
# al compartir la experiencia la mayoría aseguraba que los extranjeros tenían mayor presupuesto que los chiles.
# Junto con esto el análisis oficial entrego que los turistas extranjeros gastaron mucho más que los nacionales.
#  en función de comparar con las cifras oficiales y la percepción de los encuestadores se plantea la pregunta de investigación 
# ¿ La porción de chilenos y extranjeros con respecto a su presupuesto  diario fue diferente?.




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
# ha de determinar el tamaño de muestra necesitado para realizar esta comparación estadística utilizando
# muestreo sistemático.


# El tamaño de una muestra se le conoce como aquel número determinado de sujetos o cosas que componen la muestra extraída 
# de una población, La cosa no es tan sencilla como aumentar n indefinidamente. Conforme el tamaño
# de la muestra se hace más grande, también lo hacen el coste económico, el tiempo
# necesario y los inconvenientes del trabajo de campo. Así que las soluciones perfectas no
# existen en esta dimensión y siempre hay que manejar variables que apuntan a
# direcciones diferentes. El objetivo es conseguir un n razonable en tres sentidos: apunta a
# un nivel de seguridad razonable, con una precisión razonable y unos recursos razonables.
# por lo cual para este trabajo el nivel de seguridad ( o porcentaje de error )  es del 5% debido a la cantidad
# de encuestas a tomadas es baja (bajo 300 encuestas)
# se utiliza un 95 % de confianza tomando en cuenta la importancia del resultado  y los estandares.
# los recursos incidieron en el numero total de población = 86.

# Fuente : http://asignatura.us.es/dadpsico/apuntes/TamMuestra.pdf

# Al utilizar Muestreo Sistematico  debemos utilizar la siguiente formula para calcular el numero de muestras:

# Formula obtenida:  VI Muestreo Sistemático, Dr. Jesús Mellado Bosque
# http://www.uaaan.mx/~jmelbos/muestreo/muapu4.pdf

# P # = Probabilidad de Ocurrencia del Fenómeno Estudiado
# Q  = Probabilidad de que no Ocurra el Fenómeno (q = 1 – p)
# Varianza = P*Q
Varianza =  # CALCULAR .... 
N = 86 # numero de la población
e= 0.05 # Maximo error permitido 5 %
D = (e^2)/4
nMuestras = (N*Varianza)/((N-1)*D + Varianza)
nMuestras <- ceiling(nMuestras) # aproximar hacia arriba
cat("Numero de muestras a utilizar :")
cat(nMuestras)


# Dias de nacimiento

# Fernanda Muñoz: d1 = 4
# Jorge Plaza: d2 = 13
# Felipe Vasquez: d3 = 17
# Nicolas Gutierrez: d4 = 17
# Se calcula la semilla con los dias de nacimientos de los integrantes mencionados anteriormente
# Semilla: d1 * d2 + d3 * d4 = 4 * 13 + 17 * 17 = 341
set.seed(341)


#Es una técnica de muestreos probabilísticos - 
#y que por lo tanto requiere tener un control preciso del marco muestral de individuos a seleccionar.
# Consistente en escoger  un individuo inicial de forma aleatoria entre la población y,
# a continuación, seleccionar para la muestra a cada enésimo individuo disponible en el marco muestral. 

# Pasos para la selección de un muestreo sistematico de forma manual # 

#1. Elaboramos una lista ordenada de los N individuos de la población, lo que sería el marco muestral.

#2. Dividimos el marco muestral en n fragmentos, donde n es el tamaño de muestra que deseamos. 
#El tamaño de estos fragmentos será: K=N/n, donde K recibe el nombre de intervalo o coeficiente de elevación.

#3. Número de inicio: obtenemos un número aleatorio entero A, menor o igual al intervalo. 
#Este número corresponderá al primer sujeto que seleccionaremos para la muestra dentro del primer fragmento en que hemos dividido la población.

# 4. Selección de los n-1 individuos restantes: Seleccionamos los siguientes individuos a partir del individuo seleccionado aleatoriamente,
# mediante una sucesión aritmética, seleccionando a los individuos del resto de fragmentos en 
# que hemos dividido la muestra que ocupan la misma posición que el sujeto inicial. Esto equivale a decir que seleccionaremos los individuos:

# referencias: https://www.netquest.com/blog/es/blog/es/muestreo-sistematico?fbclid=IwAR1HyAT1wP4px-mCRKxeqfi7W0bf0RR4FxvU4-SKpnTwYG4sCR0MYSFahrc

# R tiene una función llamada sys.sample que selecciona una muestra sistemática de tamaño n.

n.sys <- nMuestras
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Presupuesto, data = muestra, FUN = length)


## .. MOSTRAR MUESTRAS QUE SE TOMAR ...  ### ...
### PLOTEAR LOS RESULTADOS ### 



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

# El bootstrapping es un método de remuestreo. 
# que se utiliza para aproximar la distribución en el muestreo de un estadístico. 
# Se usa frecuentemente para aproximar el sesgo o la varianza de un estadístico, 
# Bootstrapping se basa en la ley de los grandes números , 
# que establece que si realiza una muestra una y otra vez, 
# sus datos deberían aproximarse a los datos reales de la población.
#  Esto funciona, tal vez sorprendentemente, incluso cuando está utilizando una sola muestra para generar los datos.
# La enorme potencia de cálculo de los ordenadores actuales facilita considerablemente 
# la aplicabilidad de este método tan costoso computacionalmente.
#Esta técnica resulta especialmente útil en aquellas situaciones en las que las muestras con las que se cuenta son pequeñas 
# Como es en este caso.

#fuentes :https://www.statisticshowto.datasciencecentral.com/bootstrap-sample/?fbclid=IwAR3BUYPJN-4EAxDltH3HyYROns1OUuDA-iN1Gmk6J-IGWS04bEBE5BSFd9s




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



#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######
#### DEFENIR MEJOR #######

# procedimiento ## 


# Se tiene una "tabla de dos vías"  que registra las
# frecuencias observadas para las posibles combinaciones de dos
# variables categóricas.  Para esto existe un procedimiento χ^2, que se le conoce de forma 
# Prueba χ^2 de Independencia. En donde hay dos factores ("Presupuesto" y "Procedencia")
# que se miden en una misma población


cat("\n")

cat("HO: la procedencia de una persona no depende del presupuesto")
cat("HA: la procedencia incide en el presupuesto \n")


# Se define un alpha de 0.05 estandar debido a que  es razonable para esta prueba, dado la cantidad de datos obtenidos y lo que se busca (no critico).
alpha <- 0.05  # es decir 95% confianza.




# En r la función chisq.test realiza una prueba chi-cuadrado, utilizaremos esto para realizar una prueba de independecia  χ^2.


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


