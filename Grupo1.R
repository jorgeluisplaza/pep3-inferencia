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


# .packages("ez") # InstalaciÃ³nn del paquete "Ez"


# .packages("tidyverse") # InstalaciÃ³nn del paquete "tidyverse"


# .packages("WRS2") # InstalaciÃ³n del paquete "WRS2"

library(ez)
library(magrittr)
library(dplyr)
library(purrr)
library(WRS2)
library(ggpubr)
library(ggplot2)
library(knitr)
library(boot)
library(tidyr)
library(devtools)
#_github("DFJL/SamplingUtil")
library(SamplingUtil)

library(stats)
library(caret)
library(pROC)

# Indicar directorio del archivo .csv
dir <- ""
#Indicar nombre del archivo
basename <- "eclipse2019.csv"
## Link de descarga: https://docs.google.com/spreadsheets/d/15mG2ATv2ArBVioZ8NSSuixG7Fr_cX8iw_0K3thaGz_M/edit?usp=sharing #
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
# muestreo sistemático.


# El tamaño de una muestra se le conoce como aquel número determinado de sujetos o cosas que componen la muestra extraída 
# de una población, La cosa no es tan sencilla como aumentar n indefinidamente. Conforme el tamaño
# de la muestra se hace más grande, también lo hacen el coste económico, el tiempo
# necesario y los inconvenientes del trabajo de campo. Así que las soluciones perfectas no
# existen en esta dimensión y siempre hay que manejar variables que apuntan a
# direcciones diferentes. El objetivo es conseguir un n razonable en tres sentidos: apunta a
# un nivel de seguridad razonable, con una precisión razonable y unos recursos razonables.
# por lo cual para este trabajo el nivel de seguridad ( o porcentaje de error )  es del 5% debido a la cantidad
# de encuestas a tomadas es baja (bajo 300 encuestas)
# se utiliza un 95 % de confianza tomando en cuenta la importancia del resultado  y los estandares.
# los recursos incidieron en el numero total de población = 86.

# Fuente : http://asignatura.us.es/dadpsico/apuntes/TamMuestra.pdf

# Al utilizar Muestreo Sistematico  debemos utilizar la siguiente formula para calcular el numero de muestras:

# Formula obtenida:  VI Muestreo Sistemático, Dr. Jesús Mellado Bosque
# http://www.uaaan.mx/~jmelbos/muestreo/muapu4.pdf

# P  =  25 extranjeros / 86 total 
# Q  =  61 chilenos / 86. 
# Varianza = P*Q
Varianza =  0.2061
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

# referencias: https://www.netquest.com/blog/es/blog/es/muestreo-sistematico?fbclid=IwAR1HyAT1wP4px-mCRKxeqfi7W0bf0RR4FxvU4-SKpnTwYG4sCR0MYSFahrc

# Para realizar la prueba sistematica se utiliza la funcion sys.sample de
# la libreria SamplingUnit. 

######### Importante #############

# Para el uso de la funcion se debe instalar 
# la libreria "devtools", luego descomentar la linea #_github("DFJL/SamplingUtil")
# en las librerias definidas anteriormente y ejecutar 

n.sys <- nMuestras
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1 <- aggregate(frec ~ Procedencia + Presupuesto, data = muestra, FUN = length)

## El rango de presupuesto escogidos por los investigadores
## debido al comportamiento de los datos son los siguientes:

# 1 - [0-25.000]
# 2 - [2500-50.000]
# 3 - [50.000-75.000]
# 4 - [75.000-o Mas]

# Se  calculan la cantidad de personas con sus respectivos presupuestos
presupuesto.Menor25.Chilenos <- sum(p1[c(which(p1$Presupuesto < 25000 & p1$Procedencia == "Chileno")), "frec"])
presupuesto.Menor25.Extranjeros <- sum(p1[c(which(p1$Presupuesto < 25000 & p1$Procedencia == "Extranjero")), "frec"])
presupuesto.Entre2550.Chilenos <- sum(p1[c(which(p1$Presupuesto > 25000 & p1$Presupuesto < 50000 & p1$Procedencia == "Chileno")), "frec"])
presupuesto.Entre2550.Extranjeros <- sum(p1[c(which(p1$Presupuesto > 25000 & p1$Presupuesto < 50000 & p1$Procedencia == "Extranjero")), "frec"])
presupuesto.Entre5075.Chilenos <- sum(p1[c(which(p1$Presupuesto > 50000 & p1$Presupuesto < 75000 & p1$Procedencia == "Chileno")), "frec"])
presupuesto.Entre5075.Extranjeros <- sum(p1[c(which(p1$Presupuesto > 50000 & p1$Presupuesto < 75000 & p1$Procedencia == "Extranjero")), "frec"])
presupuesto.Mayor75.Chilenos <- sum(p1[c(which(p1$Presupuesto > 75000 & p1$Procedencia == "Chileno")), "frec"])
presupuesto.Mayor75.Extranjeros <- sum(p1[c(which(p1$Presupuesto > 75000 & p1$Procedencia == "Extranjero")), "frec"])

# Se agregan los presupuestos de Chilenos y Extranjeros
presupuesto.Menor25 <- c(presupuesto.Menor25.Chilenos, presupuesto.Menor25.Extranjeros)
presupuesto.Entre2550 <- c(presupuesto.Entre2550.Chilenos, presupuesto.Entre2550.Extranjeros)
presupuesto.Entre5075 <- c(presupuesto.Entre5075.Chilenos, presupuesto.Entre5075.Extranjeros)
presupuesto.Mayor75 <- c(presupuesto.Mayor75.Chilenos, presupuesto.Mayor75.Extranjeros)

# Se guarda en un data frame
table.p1 <- data.frame(
  "[0, 25000]"=presupuesto.Menor25, 
  "[25000, 50000]"=presupuesto.Entre2550, 
  "[50000, 75000]"=presupuesto.Entre5075,
  "> 75000"=presupuesto.Mayor75
)

frecuencias <- aggregate(frec ~ Procedencia + Presupuesto, data = muestra, FUN = length)

# Se definen el nombre de las filas 
rownames(table.p1) <- c("Chileno", "Extranjero")

# El bootstrapping es un método de remuestreo. 
# que se utiliza para aproximar la distribución en el muestreo de un estadístico. 
# Se usa frecuentemente para aproximar el sesgo o la varianza de un estadístico, 
# Bootstrapping se basa en la ley de los grandes números , 
# que establece que si realiza una muestra una y otra vez, 
# sus datos deberían aproximarse a los datos reales de la población.
#  Esto funciona, tal vez sorprendentemente, incluso cuando está utilizando una sola muestra para generar los datos.
# La enorme potencia de cálculo de los ordenadores actuales facilita considerablemente 
# la aplicabilidad de este método tan costoso computacionalmente.
# Esta técnica resulta especialmente útil en aquellas situaciones en las que las muestras con las que se cuenta son pequeñas 
# Como es en este caso.

#fuentes :https://www.statisticshowto.datasciencecentral.com/bootstrap-sample/?fbclid=IwAR3BUYPJN-4EAxDltH3HyYROns1OUuDA-iN1Gmk6J-IGWS04bEBE5BSFd9s




# Se define estadistico para el bootstraping
# Para cada iteracion se hace calculo de tabla
# y de la prueba Chi - Squared
# Se devuelve el estadistico obtenido

chisqr <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1 <- aggregate(frec ~ Procedencia + Presupuesto, data = data, FUN = length)
  presupuesto.Menor25.Chilenos <- sum(p1[c(which(p1$Presupuesto < 25000 & p1$Procedencia == "Chileno")), "frec"])
  presupuesto.Menor25.Extranjeros <- sum(p1[c(which(p1$Presupuesto < 25000 & p1$Procedencia == "Extranjero")), "frec"])
  presupuesto.Entre2550.Chilenos <- sum(p1[c(which(p1$Presupuesto > 25000 & p1$Presupuesto < 50000 & p1$Procedencia == "Chileno")), "frec"])
  presupuesto.Entre2550.Extranjeros <- sum(p1[c(which(p1$Presupuesto > 25000 & p1$Presupuesto < 50000 & p1$Procedencia == "Extranjero")), "frec"])
  presupuesto.Entre5075.Chilenos <- sum(p1[c(which(p1$Presupuesto > 50000 & p1$Presupuesto < 75000 & p1$Procedencia == "Chileno")), "frec"])
  presupuesto.Entre5075.Extranjeros <- sum(p1[c(which(p1$Presupuesto > 50000 & p1$Presupuesto < 75000 & p1$Procedencia == "Extranjero")), "frec"])
  presupuesto.Mayor75.Chilenos <- sum(p1[c(which(p1$Presupuesto > 75000 & p1$Procedencia == "Chileno")), "frec"])
  presupuesto.Mayor75.Extranjeros <- sum(p1[c(which(p1$Presupuesto > 75000 & p1$Procedencia == "Extranjero")), "frec"])
  
  presupuesto.Menor25 <- c(presupuesto.Menor25.Chilenos, presupuesto.Menor25.Extranjeros)
  presupuesto.Entre2550 <- c(presupuesto.Entre2550.Chilenos, presupuesto.Entre2550.Extranjeros)
  presupuesto.Entre5075 <- c(presupuesto.Entre5075.Chilenos, presupuesto.Entre5075.Extranjeros)
  presupuesto.Mayor75 <- c(presupuesto.Mayor75.Chilenos, presupuesto.Mayor75.Extranjeros)
  
  # Se guarda en un data.frame
  table.p1 <- data.frame(
    "[0, 25000]"=presupuesto.Menor25, 
    "[25000, 50000]"=presupuesto.Entre2550, 
    "[50000, 75000]"=presupuesto.Entre5075,
    "> 75000"=presupuesto.Mayor75
  )
  
  rownames(table.p1) <- c("Chileno", "Extranjero")
  chi <- chisq.test(table.p1)
  return(chi$statistic)
}

# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
modelo.boot <- boot(muestra, chisqr, R = n.perm)

distribucion <- modelo.boot$t

## procedimiento ##

# Se tiene una "tabla de dos vías"  que registra las
# frecuencias observadas para las posibles combinaciones de dos
# variables categóricas.  Para esto existe un procedimiento χ^2, que se le conoce de forma 
# Prueba χ^2 de Independencia. En donde hay dos factores ("Presupuesto" y "Procedencia")
# que se miden en una misma población

# Para la realizacion de esta prueba se necesita una condicion

# 1 - El tamaño de la muestra debe ser mayor que 5

# Se cumple la condicion sabiendo que el n estudiado es 69


cat("\n")

# Se plantean las hipotesis correspondientes a la prueba de independencia

cat("HO: la procedencia de una persona no depende del presupuesto")
cat("HA: la procedencia incide en el presupuesto \n")

# Volver a resaltar que para la veracidad de alguna de estas pruebas de utiliza
# un estudio de las proporciones

# Se define un alpha de 0.05 estandar debido a que  es razonable para esta prueba, dado la cantidad de datos obtenidos y lo que se busca (no critico).
alpha <- 0.05 # es decir 95% confianza.

# En r la función chisq.test realiza una prueba chi-cuadrado, utilizaremos esto para realizar una prueba de independecia  χ^2.

# Se obtiene el valor observado
observado <- chisq.test(table.p1)$statistic

# Se obtiene el intervalo de confianza
p.95 <- (1 - alpha)*n.perm
distribucion <- sort(distribucion)
limit <- distribucion[p.95]

# Se grafica y se muestran los valores observados y obtenidos
histograma <- gghistogram(
  data = data.frame(dist = distribucion),
  x = "dist",
  fill = "lightblue",
  xlab = "Chi cuadrado",
  ylab = "Frecuencia",
  title = "Distribucion χ2 usando Bootstrap",
  bins = 35
) + geom_vline(
  xintercept = observado,
  linetype = "solid", color = "blue"
) + geom_vline(
  xintercept = limit,
  linetype = "solid", color = "red"
)

plot(histograma)
####### Para el calculo del p - valor se estudia los valores obtenidos
# por la distribucion y el observado, para esto se cuenta la cantidad de 
# veces que el valor es mayor y se obtiene el calculo del p - valor
# sobre la cantidad de repeticiones realizadas con la prueba de bootstrap

# Se calcula el p-valor
count2 <- sum(distribucion > observado)
p.value <- (count2 + 1)/(n.perm + 1)

if(p.value < alpha){
  cat("La prueba es significativa para una significancia de: ", 1-alpha, " y un p-valor de: ", p.value , "\n")
  cat("Hay suficiente evidencia para rechazar la hipótesis nula (H0) en favor de la alternativa (HA)\n")
  cat("Se concluye el presupuesto de una persona verá el eclipse es dependiente de la procedencia (Chileno o Extranjero)")
} else {
  cat("La prueba no es significativa para una significancia de: ", 1-alpha, " y un p-valor de: ", p.value ,"\n")
  cat("No hay suficiente evidencia para rechazar la hipótesis nula\n")
  cat("Se concluye que el presupuesto de una persona que verá el eclipse es independiente de la procedencia (Chileno o Extranjero)")
}

# La prueba concluye que no hay evidencia suficiente para rechazar la hipotesis nula.
# Es decir, lo planteado por los investigadores no se puede confirmar debido a que 
# la prueba nos dice que el gasto de presupuesto diario de una persona
# no depende de la procedencia de la misma

############### FIN PREGUNTA 1 ######################

############### PREGUNTA 2 ######################

# GRUPO 1

# Fernanda MuÃ±oz: d1 = 4
# Jorge Plaza: d2 = 13
# Felipe Vasquez: d3 = 17
# Nicolas Gutierrez: d4 = 17


set.seed(4*17+13*17)


#Pregunta 2
#El equipo debe plantear una pregunta de investigación interesante que requiera la comparación de más de
#dos medias.
#Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
#ha de determinar el tamaño de muestra requerido para realizar esta comparación estadística utilizando
#muestreo estratificado con afijación proporcional.
#Usando la semilla d1·d3+ d2·d4, el equipo debe aplicar este muestreo en los datos de Coquimbo.
#El equipo debe usar un método robusto para responder su pregunta de investigación.




########
#El equipo debe plantear una pregunta de investigación interesante que requiera la comparación de más de
#dos medias.
######

# Segun Cifras oficiales las ventas  durante el eclipse en los supermercados de la zona habrían aumentado en cerca de 30% sus ventas. 
# Fernando Guzmán, presidente de la Cámara de Comercio de Coquimbo, coincidió y afirmo que: 
# “Aún no hemos sacado todas las cuentas, pero hasta el momento hemos visto que el mayor flujo de turistas fue domingo, lunes y martes. 
# Nos vimos altamente beneficiados por la ocupación hotelera, ya que hostales, hoteles y residenciales estuvieron copados. 
# Los presupuestos fueron satisfechos también por los restaurantes y locales de comida”, dijo.
# Fuente: Diario el Día - http://www.diarioeldia.cl/economia/turismo/eclipse-turistas-se-quedaron-en-promedio-3-dias-gastaron-99-mil-diarios

# Bajo este contexto, durante el trabajo en terreno, una de las preguntas a los turista fue  como se iban a alimentar durante su estadia. 
# Las opciones que se les dieron a los turistas para escoger fueron:
#   Restaurant :    Los turistas preferian ir a comer a Restaurant
#   Cocina :        Los turistas prefieren cocinarse ellos mismos.
#   Comida Rapida:  Los turistas prefieren ir a comer a locales de comida 
#                   rápida.
#   Otro:           Los turistas prefieren  otra opción para alimentarse

# A simple vista las respuestas fueron variadas, pero intuitivamente dentro del grupo se propuso que el promedio de las personas 
# que comen en restaurant era mayor al promedio de personas que se cocinaban, debido a que tenian un mayor presupuesto para gastar.
# Es por esto que se plantea como pregunta de investigación si la media de 
#los presupuestos eran diferentes dependiendo del medio para alimentarse.




####---------------------------------------------------------------####
#### PARTE 1 : MUESTREO ESTRATIFICADO CON AFIJACIÃ“N PROPORCIONAL   ####
####---------------------------------------------------------------####





#Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
#ha de determinar el tamaño de muestra requerido para realizar esta comparación estadística utilizando
#muestreo estratificado con afijación proporcional.



# Antes de continuar con el procedimiento, es necesario tener claro
# la muestra que se utilizarÃ¡.
# Una buena forma de obtener un numero de muestras es utilizar
# un muestreo estratificado con afijaciÃ³n proporcional.
# Concepto: MUESTREO ESTRATIFICADO
# El muestreo es una herramienta de investigaciÃ³n cientÃfica, cuya funciÃ³n
# bÃ¡sica es determinar que parte de una poblaciÃ³n debe examinarse, con la finalidad de hacer
# inferencias sobre dicha poblaciÃ³n. El muestreo estratificado es una tÃ©cnica de muestreo 
# probabilÃstico en donde el investigador divide a toda la poblaciÃ³n en diferentes subgrupos 
# o estratos. Luego, selecciona aleatoriamente a los sujetos finales de los diferentes estratos 
# en forma proporcional. 
# Â¿Por quÃ© conviene utilizar este tipo de muestreo?
# -> Puede entregar informacion mas precisa al reducir el error por cada estrato.
# -> Se obtienen muestras mÃ¡s representativas
# -> Permite inferir para cada estrato de manera independiente

# Condiciones: 
# -> La poblaciÃ³n estÃ¡ dividida en estratos (subpoblaciones)
# -> Se conocen las proporciones o tamalos de los distintos estratos

# Muestreo estratificado: Con afijacion proporcional
# Este tipo de muestreo el tamaÃ±o de cada estrato en la muestra es proporcional al tamaÃ±o de cada
# estrato en la poblaciÃ³n.
#                                 n_{h} = n * W_{h}
#
# Donde: 
# n_{h} : TamaÃ±o del estrato h
# n : tamaÃ±o de la poblaciÃ³n
# W_{h} : Peso del estrato h ;     W_{h} = N_{h}
#                                        -------
#                                           N  




# Antes de continuar con el procedimiento, es necesario tener claro la muestra que se utilizará.
# Una buena forma de obtener un numero de muestras es utilizar un muestreo estratificado con afijación proporcional.

#-----------------------#
# 1.1  Captura de Datos #
#-----------------------#


data.pregunta2 <- read.csv("eclipse2019.csv", header = TRUE) # lectura

# Se filtran la cantidad de persona que escogieron cada opciÃ³n

Alimentacion <- data.frame(Alimentos = data.pregunta2$Alimentacion)
Alimentacion.frame <- Alimentacion %>% 
  group_by(Alimentos) %>% summarise(Cantidad = length(Alimentos))


# Resultado:

# 1 Cocina              35
# 2 Comida Rapida       12
# 3 Otro                12
# 4 Restaurant          27
#                     ------
#                       86    


#---------------------------------------------------------#
# 1.2  AplicaciÃ³n de la formula de muestreo estratificado #
#---------------------------------------------------------#



# Puesto que la formula para obtener el numero de muestras es :

#   muestras   =        S( Ni * Pi * Qi )
#                   -------------------------
#                   N * E + S( Ni * Pi * Qi )
#                           -----------------
#                                   N
# Donde :
#
#     N  = TamaÃ±o total.
#     Ni = TamaÃ±o del estrato i.
#     Pi = Peso del estrato i. Se obtiene dividiendo Ni / N .
#     Qi = Complemento de Pi. Se calcula como  (1-Pi).
#     E = Es el error. Esta dado por :

#             E   =       (error maximo permitido)^2
#                    ------------------------------------
#                     (Valor de dist. Z con (1 - a/2))^2





# 1.2.1 Numerador
#----------------

# Primeramente comencemos armando el numerador.

# Definamos entonces, el tamaÃ±o de N que es la suma de estratos: 
N <- sum(Alimentacion.frame$Cantidad)

# Comenzamos a agregarles datos al frame, como el peso de
# Cada estrato (Pi):


Alimentacion.frame$Peso <- ( Alimentacion.frame$Cantidad  / 
                               sum(Alimentacion.frame$Cantidad) 
)

# AÃ±adimos el complemento del peso, osea 1-peso (Qi):

Alimentacion.frame$Complemento <- (1 - Alimentacion.frame$Peso)

# Con esto ya tenemos los datos necesarios para el numerador
# en la formula para obtener el tamaÃ±o de la muestra.

# entonces el numerador estarÃ¡ dado por : S( Ni * Pi * Qi )

Alimentacion.frame$NiPiQi <- (Alimentacion.frame$Cantidad *
                                Alimentacion.frame$Peso *
                                Alimentacion.frame$Complemento ) 

numerador <- sum(Alimentacion.frame$NiPiQi)

# Antes de pasar a la parte del denominador lo que tenemos 
# de momento es : 

#                   N          Pi         Qi       
#   Alimentos    Cantidad    Peso    Complemento  NiPiQi

# 1 Cocina          35      0.407       0.593     8.45
# 2 Comida Rapida   12      0.140       0.860     1.44
# 3 Otro            12      0.140       0.860     1.44
# 4 Restaurant      27      0.314       0.686     5.82


# Por lo que ahora corresponde a calcular el denominador




# 1.2.2 Denominador
#------------------


# Para ello debemos fijar dos valores:

# Un error maximo acepable

error.maximo <- 0.05

# y un alpha 

alpha <- 0.05

# Lo que sigue es calcular el error de la muestra, lo cual esta dado
# por :

error <- ( error.maximo^2 / (qnorm(1-(alpha/2)))^2 )

# recordemos que el total de la poblaciÃ³n es N.

# entonces ya con esto tenemos nuestro denominador:

denominador <- (N * error) + ( sum(Alimentacion.frame$NiPiQi) / N)

# Finalmente el total de las muestras que debemos tener es
# (redondeando al supremo):

muestras.p2 <- ceiling(numerador / denominador )

# Como resultado nos da 68. 

# Ahora si queremos saber cuantas muestras por estratos necesitamos:

Alimentacion.frame$Muestras <- (Alimentacion.frame$Peso *
                                  muestras.p2)

# aun que falta redondear estas ultimas ya que tenemos:

# Alimentos            Muestras

# 1 Cocina               27.7
# 2 Comida Rapida        9.49
# 3 Otro                 9.49
# 4 Restaurant           21.3
#                      -------
#                         68



#-------------------------------------------#
# 1.3  Redondeo de muestras en los estratos #
#-------------------------------------------#


# El redondeo actual estÃ¡ dado, por la funcion de redondeo al valor de 5

redondeo.actual <- sum(round(Alimentacion.frame$Muestras))

# Entonces Â¿Como redondear esto ?

# Crearemos una funciÃ³n especial para redondear

customRound <- function(value, vector){
  
  # obtenemos los enteros
  vector.int <- floor(vector)
  new.vector <- vector
  
  # se obtiene solo el valor decimal haciendo la resta
  vector.float <- vector - vector.int
  
  # Se obtienen las posiciones de los valores mayores y menores a 
  # value
  no.round <- which(vector.float < value)
  round <- which(vector.float > value)
  
  # Si es mayor se fija un 1, si es menor se fija un 0
  new.vector[no.round] = 0
  new.vector[round] = 1
  # se suma el vector con los enteros mas un vector de 0 y 1
  vector.result <- (floor(vector) + new.vector)
  
  return (vector.result)
}



# Dependiendo del numero de muestras teoricas con el redondeo normal

if (muestras.p2 > redondeo.actual) {
  Alimentacion.frame$Muestras <- customRound(0.4,
                                             Alimentacion.frame$Muestras)
} else if (muestras.p2 < redondeo.actual){
  
  Alimentacion.frame$Muestras <- customRound(0.7,
                                             Alimentacion.frame$Muestras) 
}else { #Si ambos valores son iguales, entonces ocupados el redondeo normal
  Alimentacion.frame$Muestras <- round(Alimentacion.frame$Muestras)
}


# Finalmente con este redondeo obtenemos valores mejores: 

# Alimentos            Muestras

# 1 Cocina               28
# 2 Comida Rapida        10
# 3 Otro                 10
# 4 Restaurant           21
#                      ------
#                        69

# Cabe destacar que como el valor de 2 estratos son iguales, no podemos
# obtener una aproximaciÃ³n al valor de la muestra exacta.
# Por lo que tomamos 69 muestras en vez de 68. 



#-----------------------------#
# 1.4  CreaciÃ³n de la Muestra #
#-----------------------------#


# ahora si contamos con todos los datos casi exactos.
# por lo que podemos crear nuestra muestra: 


# se crea un nuevo data frame  con solo alimento e ingresos
data.poblacion.p2 <- data.frame(
  Alimento = factor(data.pregunta2[["Alimentacion"]]),
  Ingresos = data.pregunta2[["Presupuesto"]]
)

# Ordenamos la poblaciÃ³n por orden alfabetico
data.poblacion.p2 <- data.poblacion.p2[
  order(data.poblacion.p2$Alimento),]  

# Ahora comenzamos a escoger al azar con la seed fijada

#Creamos una variable data.pregunta2.muestra
data.muestra.p2 <- data.poblacion.p2 %>% # De la poblaciÃ³n 
  
  group_by(Alimento) %>% # Agrupapos por alimentos
  
  nest() %>% # dejamos todos los datos en 1 fila por alimento
  
  mutate(n=Alimentacion.frame$Muestras) %>% # fijamos el tamaÃ±o 
  # de cada muestra
  
  mutate(samp = map2(data, n, sample_n)) %>% # Mapeamos una funciÃ³n 
  # de sample segun tamaÃ±o
  
  select(Alimento, samp) %>% # Aplicamos la funciÃ³n 
  
  unnest() %>% # revertimos la agrupaciÃ³n de filas (ahora hay n filas)
  
  as.data.frame() # volvemos a transformar a data.pregunta2 frame




####-------------------------------------------####
#### PARTE 2 : APLICACIÃ“N DEL METODO ROBUSTO   ####
####-------------------------------------------####

#Los métodos robustos son aquellos que permiten vislumbrar qué tan efectivo 
#son las hipótesis planteadas a partir de los datos presentes que pueden ser o no
# de caracter anómalo, es decir, que existen valores atípicos dentro de la muestra.
#También predicen que tan efectivo son los métodos paramétricos y/o
#no paramétricos para estimar la media, la mediana o la varianza de la muestra, 
#(Para el ejercicio, se debe estimar la media).

#Existen 4 tipos de metodos robustos:
#- Estimador de Huber
#- Media α-recortado muestral
#- Media α-winsorizada muestral
#-Mediana muestral

#Dentro de las bondades de R, existen paquetes que permiten aplicar algunos métodos robustos,
#como por ejemplo, el paquete "WRS2" nos facilita varias 
# funciones para utilizar metodos robustos.
# Por las condiciones del problema se podria sospechar que el metodo 
# ANOVA es el indicado para comparar varias muestras. Incluso la 
# alternativa no parametrica a ANOVA -> Kruskal.


# Una alternativa para el metodo robusto de ANOVA  t1way() o t1waybt() 
# con bootstraping (dependiendo de los datos), ya que este ésta función lo que hace es recortar una
#porción de la muestra por un lado para eliminar los datos atípicos de la muestra. :


#-----------------------------#
# 2.1  Analisis de datos      #
#-----------------------------#

# Analisis como son los datos primeramente: 


# 2.1.1 Independencia
#--------------------


# Sabemos que los datos fueron tomados de forma independiente y al azar
# ya que fueron tomados en diferentes partes de La serena.


# 2.1.2 Normalidad
#-----------------

# Para ver la normalidad de los ingresos de las personas segun donde van 
# a comer, podemos generar un grafico QQ para ver como se comportan los
# datos.

# se crea una grafica de ingresos vs alimento
normalidad.p2 <- ggqqplot(
  data.muestra.p2,
  x = "Ingresos",
  color = "Alimento"
)

# se aÃ±aden todos 
normalidad.p2 <- normalidad.p2 + facet_wrap(~ Alimento)
print(normalidad.p2)

# Podemos ver claramente que los datos que manejamos no son normales.

# incluso si vemos los datos en un grafico de cajas: 

alimentos.diagrama.p2 <- ggboxplot(
  data.muestra.p2,
  x = "Alimento", y = "Ingresos",
  xlab = "Alimento",
  color = "Alimento",
  add = "jitter"
)
print(alimentos.diagrama.p2)

# podemos ver mucha presencia de outliers.

# Conciderando esto podemos decir que el metodo robusto nos viene como 
# anillo al dedo. Ya que como los datos son muy distintos o atipicos.
# La medicion cambiarÃ¡ si se le agrega o quita un dato.


#------------------------------#
# 2.2  AplicaciÃ³n del metodo   #
#------------------------------#

# Como la alternativa robusta de ANOVA es, segun las lecturas, es 
# t1way() ya que este metodo compara diferentes medias de 1 via 
# como ANOVA


# 2.2.1 Hipotesis
#-----------------

# Antes de proceder con el metodo, veamos cuales son las hipotesis
# a contrastar : 

# H0 =  Âµ_ingresos_restaurant = Âµ_ingresos_cocina =
#       Âµ_ingresos_omida_rapida = Âµ_ingresos_otro

# H1 = Al menos un par de medias son distintas. 


# Dentro de este estudio tambien es importante definir un alpha.

# Como siempre manejaremos este estudio con un error del 5%. 


# 2.2.2 AplicaciÃ³n
#-----------------

# entonces finalmente:


# veamos nuestra prueba:
prueba.robusta.p2 <- t1way(Ingresos ~ Alimento, data.muestra.p2)

# Como resultado obtenemos : 

# Test statistic: F = 2.4 
# Degrees of freedom 1: 3 
# Degrees of freedom 2: 14.92 
# p-value: 0.10871 

# Explanatory measure of effect size: 0.39


# veamos la misma prueba pero ahora con bootstrap:
prueba.robusta.p2_v2 <- t1waybt(Ingresos ~ Alimento, data.muestra.p2)

# obtemos : 

# Effective number of bootstrap samples was 472.

# Test statistic: 2.4 
# p-value: 0.09534 
# Variance explained 0.14 
# Effect size 0.374 


# 2.2.3 Analisis de Resultados
#-----------------------------


# como en ambos casos el p-valor fue mayor al alpha entregado,
# no rechazamos H0, por lo que podemos decir que el promedio
# entre los grupos si es igual o al menos similar. 

# Esto quiere decir que dado los datos recopilados en el estudio
# y segun las pruebas hechas, no hay evidencia que nos diga que el
# promedio de ingresos por persona que comieron en diferentes lugares
# es distinto.

#Además se confirma que el método ocupado para verificar la no existencia de diferencia de medias 
#de los turistas que se alimentaron de diferentes formas es el adecuado
#------------------------------------------------------------------------

########## FIN PREGUNTA 2 #############



#-----------------------------------------------------------------
#-----------------------------------------------------------------
#-----------------------------------------------------------------

########## PREGUNTA 3 ###########

# Indicar directorio del archivo .csv
#dir <- "C:/Users/Acer/Desktop/pep3/pep3-inferencia"

# Se hace calculo de la semilla con respecto a los dias de nacimiento de los investigadores mencionados previamente
# Semilla: d1 * d4 + d2 * d3 = 4 * 13 + 17 * 17 = 52 + 289 = 341
set.seed(341)

# Indicar nombre del archivo
basename <- "eclipse2019.csv"

# Se lee el archivo   
#file <- file.path(dir,basename)

# Se guardan los datos en una tabla
datos.todos <- read.csv (
  file = basename,
  sep = ",",
  encoding = "UTF-8"
)

tabla <- data.frame(
  Procedencia=datos.todos$Procedencia,
  Edad=datos.todos$Edad,
  Noches=datos.todos$Noches,
  Presupuesto=datos.todos$Presupuesto
)

# Se puede notar que hay mucho menos extranjeros que chilenos, por lo que hay 
# que tomar numero de muestras proporcionales a el mejor de ellos. 
table(tabla$Procedencia)

chilenos <- c(which(tabla$Procedencia == "Chileno"))
extranjeros <- c(which(tabla$Procedencia == "Extranjero"))
# Se deben tener una cantidad parecida (igual) entre chilenos y extranjeros
proporcion <- 0.9
nmax <- min( length(extranjeros), length(chilenos))*proporcion

# Se utiliza muestreo simple aleatorio para la obtencion de la muestra
indices.chilenos <- sample( chilenos, nmax)
indices.extranjeros <- sample( extranjeros, nmax)

muestra.chilenos <- tabla[ indices.chilenos, ]
muestra.extranjeros <- tabla[ indices.extranjeros, ]
muestra <- rbind(muestra.chilenos, muestra.extranjeros)

# Para realizar el modelo automático de R
muestra.completa.chilenos <- datos.todos[ indices.chilenos, ]
muestra.completa.extranjeros <- datos.todos[ indices.extranjeros, ]
muestra.completa <- rbind(muestra.completa.chilenos, muestra.completa.extranjeros)
######## Análisis de modelos ############

# Se eligen variables predictoras para el modelo logístico.
# Hay 3 candidatos posibles que se sugieren en base a la experiencia encuestando
# 
# 1. Edad: 
#     Se notaba que la mayoría de los jóvenes eran chilenos, mientras que era común
#     encontrarse con extranjeros de mayor edad.
# 2. Presupuesto:
#     También se encuesto a varios extranjeros que contaban con un alto presupuesto
#     en comparación con algunos chilenos. Podría haber correlación con el número de noches
# 3. Noches:
#     Se pensaba que por lo general, un extranjero vendría a chile por una cantidad mas
#     extensa de tiempo, en vez de realizar un viaje esporádico. Mientras que un chileno
#     puede estacionarse en el lugar con mayor libertad. Por otro lado, los datos no resultaron
#     como se esperaba. Se incluye de igual manera para observar su influencia en el modelo.

# Comenzando con el modelo nulo, se van construyendo los modelos y observando su AIC y p-valor
nulo <- glm(
  Procedencia ~ 1,
  family = binomial(link = "logit"),
  data = muestra
)
print(summary(nulo))
# Se tiene un AIC base de 62.997

# Ahora, incluyendo la edad al modelo:
modelo.edad <- glm(
  Procedencia ~ Edad, 
  family=binomial(link='logit'), 
  data=muestra, 
  na.action = na.omit
)
print(summary(modelo.edad))

# Se realiza una comparación de la desviación usando anova
# La hipotesis nula de esta comparación es que no hay mejoras significativas
# en agregar la edad al modelo. En este caso se rechaza esta hipótesis (P < 0.001)
# por lo que existen mejoras en el modelo con la Edad. 
comparacion <- anova(nulo, modelo.edad, test = "LRT")
print(comparacion)

# Se mejora a un AIC de 50.8
# Se mantiene la edad como un predictor efectivo.

# Ahora incluyendo las noches:
modelo.edad.noches <- update(modelo.edad, . ~ . + Noches)
print(summary(modelo.edad.noches))

# Se obtiene un AIC mayor al anterior!. Más aún, la comparación de modelos
# Tampoco resulta significativa por lo que no hay mejoras en el modelo
# Al incluir las Noches. Se opta por no incluirlo
comparacion <- anova(modelo.edad, modelo.edad.noches, test = "LRT")
print(comparacion)

# Ahora incluyendo el presupuesto:
modelo.edad.presupuesto <- update(modelo.edad, . ~ . + Presupuesto)
print(summary(modelo.edad.presupuesto))

# Se mejora el AIC a un  44.3. Aunque un P-valor de 17.8 no resulta muy confiable
# Sin embargo, en cuanto a la mejora en comparación con el modelo anterior
# resulta en una mejora significativa. Se incluye en el modelo
comparacion <- anova(modelo.edad, modelo.edad.presupuesto, test = "LRT")
print(comparacion)

# Se queda con el modelo Procedencia ~ Edad + Presupuesto
modelo <- modelo.edad.presupuesto

# Revision de casos sospechosos
variables <- names(coef(modelo))[-1]
subdatos <- muestra[, c(variables, "Procedencia")]

output <- data.frame(predicted.probabilities = fitted(modelo))
output[["standardized.residuals"]] <- rstandard(modelo)
output[["studentized.residuals"]] <-rstudent(modelo)
output[["cooks.distance"]] <- cooks.distance(modelo)
output[["dfbeta"]] <- dfbeta(modelo)
output[["dffit"]] <- dffits(modelo)
output[["leverage"]] <- hatvalues(modelo)

cat("\n\n")
cat("Estadisticas de influencia para los casos\n")
cat("=========================================\n")

# Sabemos que 95% de los residuales estandarizados deberian estar entre
# -1.96 y +1.96, y que el 99% entre -2.58 y +2.58. Revisemos eso.
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1)
cat("\n\n")
cat("Residuales estandarizados fuera del 95% esperado\n")
cat("------------------------------------------------\n")
print(rownames(subdatos[sospechosos1, ]))

# Recomendaciones dicen deberiamos preocuparnos por los casos en que la
# distancia de Cook es mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuales con una distancia de Cook alta\n")
cat("-----------------------------------------\n")
print(rownames(subdatos[sospechosos2, ]))

# Tambien se recomienda revisar casos cuyo "leverage" sea mas del doble
# o triple del leverage promedio: (k + 1)/n
leverage.promedio <- ncol(subdatos) / nrow(muestra)
sospechosos3 <- which(output[["leverage"]] > leverage.promedio*2)
sospechosos3 <- sort(sospechosos3)
cat("\n\n")
cat("Residuales con levarage fuera de rango (> ")
cat(round(leverage.promedio, 3), ")", "\n", sep = "")
cat("--------------------------------------\n")
print(rownames(subdatos[sospechosos3, ]))

# Existen algunos datos con más del doble del leverage promedio

# Tambien podriamos revisar DFBeta, que deberia ser < 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------\n")
print(rownames(subdatos[sospechosos4, ]))

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort(unique(sospechosos))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------\n")
print(subdatos[sospechosos, ])
cat("\n\n")
print(summary(subdatos[sospechosos, ]))
cat("\n\n")
print(output[sospechosos, ])

# Como se tienen todos estos casos sospechosos se debe analizar que hacer con ellos
# Sacarlos del modelo o dejarlos.

# Para este caso, los casos atípicos fueron extranjeros con altos presupuestos,
# chilenos de mucha edad o extranjeros muy jóvenes.
# Estos datos corresponden a valores totalmente probables, dado el gran número de personas
# que asisten a este evento... No es posible eliminarlos de esta prueba ya que la muestra
# no es lo suficientemente grande. (Casos sospechosos fueron el 5% de toda la muestra)

# CONDICIONES DE MODELO DE REGRESION LOGISTICA
# 1) Linealidad
# 
# Dijimos que esta condicion puede verificarse revisando si existe
# interaccion significativa entre los predictores usados en un modelo
# y su transformacion logaritmica.
# [DW Hosmer, S Lemeshow (2000). Applied logistic regression. 2nd Ed.
# John Wiley & Sons,  Inc.]
# 
# Debemos crear las interacciones de cada variable con su logaritmo.
datos.log <- subdatos[, 1:2] * log(subdatos[, 1:2])
colnames(datos.log) <- paste("log_int", colnames(subdatos)[1:2], sep = ".")
datos.log <- cbind(subdatos[, 1:2], datos.log, Categoria = subdatos[[3]])
modelo.log <- glm(
  Categoria ~ .,
  family = binomial(link = "logit"),
  data = datos.log
)
cat("\n\n")
cat("Modelo con interacciones logaritmicas\n")
cat("-------------------------------------\n")
print(summary(modelo.log))

library(car)
# Requiere instalar package "rlang" adicionalmente
cat("\n\n")
cat("Prueba de Durbin?Watson para autocorrelaciones entre errores\n")
cat("------------------------------------------------------------\n")
print(durbinWatsonTest(modelo, max.lag = 5))

# En este caso, el test indica autocorrelaciones significativas
#   0 <= p-value < 2  => correlaciones positivas
#   2 < p-value <= 4  => correlaciones negativa
#   p-value = 2       => no hay correlacion
#
# Asi, debemos dudar del modelo obtenido.
# 
# En todo caso, debe tenerse cuidado con la Prueba de Durbin?Watson,
# puesto que depende del orden de los datos, por lo que reordenando
# se podria tener valores distintos. Aunque en este caso, es poco
# probable que cambie la conclusion de que existe autocorrelacion.


# Analisis de situaciones problematicas: Multicolinealidad
# --------------------------------------------------------
# 
# Dejamos pendiente la revision de la multicolinealidad, que puede 
# estropear las predicciones del modelo.
#
# Dijimos que podemos revisar esta seudo-condicion por medio del factor
# de inflacion de varianza (VIF) y el estadistico tolerancia (1 / VIF).
# Aunque no hay un acuerdo general, el valor VIF >= 10 se usa como
# umbral para preocuparse. Tambien se ha encontrado que si el VIF 
# promedio es mayor a 1, podria haber sesgo en el modelo.
# En el caso de la tolerancia, se ha sugerido que valores bajo 0.2
# podrian ser problematicos. Aunque algunos academicos creen que valores
# cercanos a 0.4 deberian ser revisados.

vifs <- vif(modelo)
cat("\n")
cat("Factores de inflacion de la varianza\n")
cat("------------------------------------\n")
print(round(vifs, 1))

#Se obtuvo un vifs de 1, no hay problemas

cat("\n")
cat("Factor de inflacion de la varianza medio\n")
cat("----------------------------------------\n")
print(round(mean(vifs), 1))

# Se obtuvo un promedio de 1, que es mayor a un 0.4, no hay problemas
tols <- 1/vifs
cat("\n")
cat("Tolerancia\n")
cat("----------\n")
print(round(tols, 2))

# Los vifs fueron 1 y por ende tolerancias de 1, que no es preocupante

# Por lo tanto se concluye que no se esta presente frente a multicolinealidad

#################### Evaluación del modelo: Predicciones ##################################

# Evaluación del modelo logístico usando predicciones
# Se crean los modelos anteriores con datos de entrenamiento, separando la muestra
# en entrenamiento y prueba.
# Se calculan los aciertos y fallos del modelo con predicciones del grupo de prueba.

# Crear particion de muestra con proporcion:

# - 70% datos de entrenamiento
# - 30% datos de prueba
Train <- createDataPartition(muestra$Procedencia, p = 0.7, list=FALSE)

training <- muestra[ Train, ]
testing <- muestra[ -Train, ]

# Modelo número 1: Procedencia ~ Edad
modelo.glm.1 <- glm(Procedencia ~ Edad, data = training, family="binomial"(link="logit"))

# Modelo número 2: Procedencia ~ Edad + Noches
modelo.glm.2 <- glm(Procedencia ~ Edad + Noches, data = training, family="binomial"(link="logit"))

# Modelo número 3: Procedencia ~ Edad + Presupuesto (Elegido)
modelo.glm.3 <- glm(Procedencia ~ Edad + Presupuesto, data = training, family="binomial"(link="logit"))

# Se obtiene la probabilidad predicha por el modelo.
# Retorna lista con la respuesta de la regresión logística (no las probabilidades)
#pred1 respuesta regresion logistica de la procedencia con la edad
#pred2 respuesta regresion logistica de procedencia, edad y noches
#pred3 respuesta regresion logistica de procedencia, edad y presupuesto
pred1 <- predict(modelo.glm.1, newdata=testing, type="response") 
pred2 <- predict(modelo.glm.2, newdata=testing, type="response")
pred3 <- predict(modelo.glm.3, newdata=testing, type="response")

# Si la predicción da menor que 50%, se responde "Chileno" (0)
# de lo contrario "Extranjero" (1)
responses.glm.1 <- ifelse(pred1 < 0.5, "Chileno", "Extranjero")
responses.glm.2 <- ifelse(pred2 < 0.5, "Chileno", "Extranjero")
responses.glm.3 <- ifelse(pred3 < 0.5, "Chileno", "Extranjero")

# Se realiza una tabla de contingencia para las predicciones. 
# Los valores de la matriz que no coinciden sus nombres
# indican errores de predicción. 

table(responses.glm.1, testing$Procedencia)
table(responses.glm.2, testing$Procedencia)
table(responses.glm.3, testing$Procedencia)

# Se calcula el porcentaje de aciertos del modelo para el conjunto
# de pruebas. Todos los modelos obtienen la misma precisión de 66.7%.
# En este caso es muy pequeño (12 casos), por lo que no resulta
# una medida confiable. 

mean1 <- round(mean(responses.glm.1 == testing$Procedencia)*100, 2)
mean2 <- round(mean(responses.glm.2 == testing$Procedencia)*100, 2)
mean3 <- round(mean(responses.glm.3 == testing$Procedencia)*100, 2)
cat("Exactitud del modelo 1 -> Procedencia ~ Edad: ", mean1, "%\n")
cat("Exactitud del modelo 2 -> Procedencia ~ Edad + Noches: ", mean2, "%\n")
cat("Exactitud del modelo 3 -> Procedencia ~ Edad + Presupuesto: ", mean3, "% \n")

# Modelo automático de R. A partir del modelo nulo (Constante)
# Agregar variables predictoras de manera ascendente por pasos.

#Estas variables son la misma muestra pero incluyendo todas las columnas 
training.modelo.R <- muestra.completa[ Train, ]
testing.modelo.R <- muestra.completa[ -Train, ]

# Modelos nulo y completo para realizar un análisis de pasos.
modelo.nulo <- glm(Procedencia ~ 1, data=training.modelo.R, family=binomial(link="logit"))
modelo.completo <- glm(Procedencia ~ ., data=training.modelo.R, family=binomial(link="logit"))

# Modelo se construye desde el modelo nulo (Procedencia ~ 1) paso por paso, según el que tenga
# un AIC menor entre todas las variables predictoras. Se limita a 3 variables.
modelo.auto <- step(
  modelo.nulo,
  direction = "both",
  scope=list(lower=modelo.nulo, upper=modelo.completo)
)
print(modelo.auto)

# Se obtuvo un AIC de 13.8, menor a un AIC de nuestro modelo 44.3

# Las variables óptimas fueron Región, Edad y Organización (Cuenta propia/Asesorado por familiar/amigo)
# Por un lado, es evidente que usar la región de precedencia no tendrá  valores atípicos para predecir
# la procedencia, ya que esta indica si son del país o no. Se coincide que la edad se incluye en el modelo.


############## Comparación de modelos por ROC #################

# Receiver Operating Characteristic (ROC)
#Constituye un método estadístico para determinar la exactitud de los modelos,
#siendo utilizadas con tres propósitos específicos: determinar el punto de corte de una escala 
#continua en el que se alcanza la sensibilidad y especificidad más alta, evaluar la capacidad discriminativa
#del modelo, es decir, su capacidad de diferenciar la procedencia del presupuesto o la procedencia con la edad, y comparar la capacidad
#discriminativa de dos o más modelos que expresan sus resultados como escalas continuas. 

#Para un modelo de proceso de punto ajustado, el ROC muestra la capacidad de la intensidad del modelo ajustado para separar 
#el dominio espacial en áreas de alta y baja densidad de puntos. La ROC no es un diagnóstico de la bondad de ajuste del modelo

#Se obtiene el roc de la pred1
roc1 <- roc(testing$Procedencia, pred1)
roc1
plot(roc1)

#Se obtiene el roc de la pred2
roc2 <- roc(testing$Procedencia, pred2)
roc2
plot(roc2)

#Se obtiene el roc de la pred3
roc3 <- roc(testing$Procedencia, pred3)
roc3
plot(roc3)

#Se hace un ROC comparativo entre cada modelos
# 1- Procedencia ~ Edad
# 2- Procedencia ~ Edad + Presupuesto
# 3- Procedencia ~ Edad + Presupuesto + Noches

roc.test(roc1,roc2)
roc.test(roc2,roc3)
roc.test(roc1,roc3)

# AUC de modelo 1 es menor a 2 y 3
# AUC de 2 y 3 son iguales


