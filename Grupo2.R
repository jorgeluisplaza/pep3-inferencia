# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara
# Integrantes: 
  # Nicolas Alarcon
  # Sandra Hernandez
  # Vicente Rivera
  # Matias Paredes

# Los datos fueron recolectados en terreno por los estudiantes
# que realizan el analisis. 
# Se basa en una encuesta a las personas que asistieron a ver 
# el eclipse total sol de la Region de Coquimbo en Chile durante
# el 2 de Julio de 2019, se estudian las siguientes variables:

## Link de descarga: https://docs.google.com/spreadsheets/d/15mG2ATv2ArBVioZ8NSSuixG7Fr_cX8iw_0K3thaGz_M/edit?usp=sharing #

# Procedencia
# Continente
# Origen
# Nacionalidad
# Edad
# Noches
# Sexo
# Transporte
# Hospedaje


#Pregunta 1
#El equipo debe plantear una pregunta de investigación interesante que requiera la comparación de dos
#proporciones. (A) 
#Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
#ha de determinar el tamaño de muestra  ( B) 
# necesitado para realizar esta comparación estadística utilizando 
#muestreo sistemático. (C) 
#Usando la semilla d1·d2+ d3·d4, el equipo debe realizar este muestreo en los datos de Coquimbo. (D)
#El equipo debe usar bootstrapping para responder su pregunta de investigación con la muestra obtenida (E)


# (A) Pregunta de investigación: El martes 2 de julio del 2019 a las 15:23 horas la luna comenzó a tapar el sol.  
# Éste fenómeno se dio en una radio de 170 kilómetros de territorio,que se extendió desde Guanaqueros a Domeyko.
# El punto de mayor atracción se dio a las 16:38 minutos, donde se produjo la total oscuridad. 
# La duración de oscuridad va a variar dependiendo de la ubicación de observación. 

# El punto central se produjo en la comuna de La Higuera, donde se extendió por 2 minutos y 36 segundos.
# En La Serena duro por 2 minutos y 15 segundos. En Vicuña, en tanto, tendrá una extensión de dos minutos y 25 segundos.
# Fuente: Diario el Día - http://www.diarioeldia.cl/region/eclipse/entre-20-segundos-236-minutos-se-extendera-oscuridad-eclipse

# El equipo una vez finalizado con las encuentas en terreno noto que la porción de extranjeros que irían a la higuera iba a ser mayor que la 
# Porción de Chilenos, por otro lado la porción de chilenos sería mayor en vicuña y la serena, que la porción de extranjeros.
# Debido a que la mayoría de los extranjeros quería presenciar el fenomeno por mas tiempo versus los chilenos que preferian la comodida,
# por ende preferian quedarse mas cerca de sus hogares o volver antes a sus lugares de origen.

# install.packages("ez") # Instalaciónn del paquete "Ez"


# install.packages("tidyverse") # Instalaciónn del paquete "tidyverse"


# install.packages("WRS2") # Instalación del paquete "WRS2"

library(ez)
library(magrittr)
library(ggpubr)
library(dplyr)
library(purrr)
library(WRS2)
library(ggplot2)
library(knitr)
library(boot)
library(tidyr)
library(devtools)
#install_github("DFJL/SamplingUtil")
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

# Se obtienen las procedencia(extranjero o Chileno) y la localidades(vicuña, la higuera y la serena) de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Localidad=datos.todos$Localidad)

#(B) 

# Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
# ha de determinar el tamaño de muestra necesitado para realizar esta comparación estadística utilizando
# muestreo sistemático.

# El tamaño de la muestra corresponde al número mínimo necesario para estimar el parámetro poblacional 
# los factores que inciden en el tamaño de la muestras son el error máximo admisible, el nivel de confianza, 
# los recursos económicos y la formula para estimar n en muestreo sistematico.
##REFERENCIAR EL LIBRO PROFE ### (factores ...)

P= 0.5 # = Probabilidad de Ocurrencia del Fenómeno Estudiado
Q = 0.5 # = Probabilidad de que no Ocurra el Fenómeno (q = 1 – p)


#  El error maximo admisible elegido fue de 0.05 debido a que como tenemos pocos datos, debemos ser menos estricto con el resultado final.
#  El nivel de confianza es de 0.05 debido a que  es razonable para esta prueba, dado la cantidad de datos obtenidos y lo que se busca (no critico).
#  Recursos economicos: Durante el estudio, el recurso económico si fue un factor, debido a que nos permitió solo estar un dia en terreno para realizar las 
#  encuestas. Esto influyó en la cantidad de encuestas tomadas, es decir, nuestro tamaño de población a evaluar.


# El Muestreo Sistematico utiliza una formula para obtener el tamaño de la muestra para estimar la porción

# Formula obtenida:  VI Muestreo Sistemático, Dr. Jesús Mellado Bosque
# http://www.uaaan.mx/~jmelbos/muestreo/muapu4.pdf


# P  =  25 extranjeros / 86 total 
# Q  =  61 chilenos / 86. 
# Varianza = P*Q  =  0.2061
varianza =  0.2061 
N = 86 # numero de la población
e= 0.05 # Maximo error permitido 5 %
D = (e^2)/4
nMuestras = (N*varianza)/((N-1)*D + varianza)
nMuestras <- ceiling(nMuestras) # aproximar hacia arriba
cat("Numero de muestras a utilizar: ", nMuestras, "\n")

#(D)
##### Semillas ######

# Dias de nacimiento
# Matias Paredes: d1 = 10
# Vicente Rivera: d2 = 10
# Nicolas Alarcon: d3 = 6
# Sandra Hernandez: d4 = 26

# Semilla: d1 * d2 + d3 * d4 = 10 * 10 + 6 * 26 = 256
set.seed(256)

#(C)

# El muestreo sistematico es un tipo de muestreo probabilístico donde se hace una selección aleatoria del primer elemento para la muestra, 
# y luego se seleccionan los elementos posteriores utilizando intervalos fijos o sistemáticos hasta alcanzar el tamaño de la muestra deseado.
# Pasos para 


# Pasos para la selección de un muestreo sistematico de forma manual #
# Definir la población objetivo.= N 
# Determinar el tamaño deseado de la muestra (n)
# Definir el marco de muestreo (N)
# Calcular el intervalo de muestreo (i) = (N)/(n). 
# Seleccionar al azar un número, r, desde  “1”  hasta i.
# Selecciona para la muestra, r, r + i, r + 2i, r ,+3i, y así sucesivamente, hasta agotar el marco.

# A nivel técnico, el muestreo sistematico no crea una muestra verdaderamente aleatoria. Sólo la selección del primer elemento de muestreo sistematico es una selección de probabilidad.
#Una vez que el primer elemento es seleccionado, algunos de los elementos tendrán una probabilidad cero de selección.

#Referencias: https://www.questionpro.com/blog/es/muestreo-sistematico/?fbclid=IwAR1i6WJv2cbS9BWSOGkk09SpyorYuBg6uUFIdgw9OSzLH051AvAKX2nqNA8


# R tiene una función llamada sys.sample que selecciona una muestra sistemática de tamaño n.

n.sys <- nMuestras    # numero de muestras # 
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)
p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = muestra, FUN = length)

####################################
#####PROCEDIMIENTO ################
####################################

# Se tiene una "tabla de dos vías"  que registra las
# frecuencias observadas para las posibles combinaciones de dos
# variables categóricas.  Para esto existe un procedimiento χ^2, que se le conoce de forma 
# Prueba χ^2 de Independencia. En donde hay dos factores ("Lugar a Visitar" y "Procedencia")
# que se miden en una misma población
# Se calculan la cantidad de personas en las tres localidades 

vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]

# Aggregate crea una tabla que cuenta (con función length) las frecuencias de cada categoría 
# a partir de datos en forma larga.
frecuencias <- aggregate(frec ~ Procedencia + Localidad, data = muestra, FUN = length)

### Gráfico de barras para visualizar frecuencias ###
ggplot(data= frecuencias, aes(x=Localidad, y=frec, fill=Procedencia)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=frec), vjust=1.6, color="Black",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_brewer(palette="Paired") + 
  ylab("Frecuencia") +
  ggtitle("Visitantes a localidades")

# Se toma la tabla anterior y se crea una tabla (matriz) de frecuencias
# Se calcula la cantidad de personas, según sea chileno o extranjero para las tres localidades
vicuña <- frecuencias[c(which(frecuencias$Localidad == "Vicuña")), "frec"]
higuera <- frecuencias[c(which(frecuencias$Localidad == "La Higuera")), "frec"]
serena <- frecuencias[c(which(frecuencias$Localidad == "La Serena")), "frec"]
# Se guarda en un data.frame
table.p1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)
# Se define el nombre de las filas 
rownames(table.p1) <- c("Chileno", "Extranjero")

# El equipo debe usar bootstrapping para responder su pregunta de investigación con la muestra obtenida.# 

# Se define estadistico para el bootstraping
# A partir del remuestreo de los índices, se construye una tabla de proporciones en cada iteración

# Boostrap: Es una muestra de remuestreo de datos dentro de una muestra aleatoria. La principal utilidad del empleo del bootstrap es reducir el sesgo dentro de #análisis o, en otras palabras, aproximar la varianza gracias a la realización de remuestreos aleatorios de la muestra inicial y no de la población.
# el procedimiento en que se basa el bootstrapping es simplemente la creación de un gran número de muestras reposicionando los datos tomando como referencia una muestra poblacional inicial. Esta técnica resulta especialmente útil en aquellas situaciones en las que las muestras con las que se cuenta son pequeñas
# Como en este caso donde las muestras a considerar son pequeñas.

# Se define estadistico para el bootstraping
# Boostrap 

chi.boot <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1 <- aggregate(frec ~ Procedencia + Localidad, data = data, FUN = length)
  vicuña <- p1[c(which(p1$Localidad == "Vicuña")), "frec"]
  higuera <- p1[c(which(p1$Localidad == "La Higuera")), "frec"]
  serena <- p1[c(which(p1$Localidad == "La Serena")), "frec"]
  tabla.obs <- data.frame(vicuña, higuera, serena)
  margen.fila <- apply(tabla.obs, 1, sum)
  margen.columna <- apply(tabla.obs, 2, sum)
  n <- sum(tabla.obs)
  tabla.esp <- margen.fila %*% t(margen.columna) / n
  difs <- (tabla.obs - tabla.esp)^2 / tabla.esp
  chi.cuadrado <- sum(difs)
  return(chi.cuadrado)
}

# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
bootobj <- boot(muestra, chi.boot, R = n.perm)
distribucion <- bootobj$t

cat("HO: la procedencia de una persona no depende del lugar a visitar\n")
cat("HA: la procedencia incide en el lugar a visitar  \n")

# Ahora, aprovechando la función creada chi.boot, se obtiene el estadístico observado
# Se puede realizar con chi.test quedando el mismo resultado.
observado <- chi.boot(muestra, 1:nrow(muestra))

# Se grafica en histograma la distribución chi cuadrado con bootstrapping
# La línea azul muestra el estadístico observado
# La línea roja muestra el extremo del intervalo de confianza (percentil 95)
grafico <- gghistogram(
  data = data.frame(Chisqr = c(observado, distribucion)),
  x = "Chisqr",
  fill = "orange",
  xlab = "Estadístico Chi cuadrado",
  ylab = "Frecuencia",
  title = "Remuestreo Bootstrap chi cuadrado",
  bins = 40
)
grafico <- grafico + geom_vline(
  xintercept = observado,
  linetype = "solid", color = "blue"
)

############# Intervalo de confianza ##################

# Se define una significancia de 0.05 estandar debido a que  es razonable para esta prueba
# dado la cantidad de datos obtenidos y lo que se busca (no critico).
alpha <- 0.05 # es decir 95% confianza.

# Se obtiene el percentil 95 del número de remuestreo + 1 (el observado)
# Se reordena la distribución y encuentra el punto crítico
p.95 <- round((1 - alpha)*n.perm + 1, 1)
distribucion <- sort(distribucion)
limit <- distribucion[p.95]

# Y se incluye en el gráfico
grafico <- grafico + geom_vline(
  xintercept = limit,
  linetype = "solid", color = "red"
)

plot(grafico)

############# P-valor y Conclusiones ###################

# Se calcula el p-valor como el porcentaje de estadísticos igual o más significativos
# que el estadístico observado en la muestra
count <- sum(distribucion > observado)
p.value <- (count + 1)/(n.perm + 1)

if(p.value < alpha){
  cat("La prueba es significativa para una significancia de: ", 1-alpha, " y un p-valor de: ", p.value , "\n")
  cat("Hay suficiente evidencia para rechazar la hipótesis nula (H0) en favor de la alternativa (HA)\n")
  cat("Se concluye que la localidad donde una persona verá el eclipse es dependiente de la procedencia (Chileno o Extranjero)")
} else {
  cat("La prueba no es significativa para una significancia de: ", 1-alpha, " y un p-valor de: ", p.value ,"\n")
  cat("No hay suficiente evidencia para rechazar la hipótesis nula\n")
  cat("Se concluye que la localidad donde una persona verá el eclipse es independiente de la procedencia (Chileno o Extranjero)")
}

####### FIN PREGUNTA 1 #########

###################
#### PREGUNTA 2 ###
################### 

# GRUPO 2

# Dias de nacimiento
# Matias Paredes: d1 = 10
# Vicente Rivera: d2 = 10
# Nicolas Alarcon: d3 = 6
# Sandra Hernandez: d4 = 26

set.seed(10*6+10*26)


#### Pregunta 2 -> Comparación de mas de 2 medias

# En la actividad realizada en la región de Coquimbo se recabaron datos 
# respecto a la edad de los turistas y cuanto iban a gastar por dia.
# Entonces see quiere saber si las personas en promedio gastaron lo
# mismo en esta visita a La serena:

# Para ello se fijaron 4 grupos de personas :

# Grupo 1 : [1-25]

# Grupo 2 : [26-50]

# Grupo 3 : [51-75]

# Grupo 4 : [76-infinito]


# Entonces con esto se quiere saber si las personas divididas en grupos
# gastaron lo mismo o existe un mayor gasto de las personas con mas edad.


####---------------------------------------------------------------####
#### PARTE 1 : MUESTREO ESTRATIFICADO CON AFIJACIÓN PROPORCIONAL   ####
####---------------------------------------------------------------####




# Antes de continuar con el procedimiento, es necesario tener claro
# la muestra que se utilizará.
# Una buena forma de obtener un numero de muestras es utilizar
# un muestreo estratificado con afijación proporcional.
# Concepto: MUESTREO ESTRATIFICADO
# El muestreo es una herramienta de investigación científica, cuya función
# básica es determinar que parte de una población debe examinarse, con la finalidad de hacer
# inferencias sobre dicha población. El muestreo estratificado es una técnica de muestreo 
# probabilístico en donde el investigador divide a toda la población en diferentes subgrupos 
# o estratos. Luego, selecciona aleatoriamente a los sujetos finales de los diferentes estratos 
# en forma proporcional. 
# ¿Por qué conviene utilizar este tipo de muestreo?
# -> Puede entregar informacion mas precisa al reducir el error por cada estrato.
# -> Se obtienen muestras más representativas
# -> Permite inferir para cada estrato de manera independiente

# Condiciones: 
# -> La población está dividida en estratos (subpoblaciones)
# -> Se conocen las proporciones o tamalos de los distintos estratos

# Muestreo estratificado: Con afijacion proporcional
# Este tipo de muestreo el tamaño de cada estrato en la muestra es proporcional al tamaño de cada
# estrato en la población.
#                                 n_{h} = n * W_{h}
#
# Donde: 
# n_{h} : Tamaño del estrato h
# n : tamaño de la población
# W_{h} : Peso del estrato h ;     W_{h} = N_{h}
#                                        -------
#                                           N  

#-----------------------#
# 1.1  Captura de Datos #
#-----------------------#


datos_p2 <- read.csv("eclipse2019.csv", header = TRUE) # lectura

# Se crea un data frame con edad e ingresos.

datos_edad_ingresos <- data.frame(Edad = datos_p2$Edad, 
                                  Ingresos = datos_p2$Presupuesto)

#  se ordenan los datos segun edad. 
datos_edad_ingresos <- datos_edad_ingresos[
  order(datos_edad_ingresos$Edad),]  


# se establecen valores para los intervalos de edad:

# Grupo 1 : [1-25]          

# Grupo 2 : [26-50]     

# Grupo 3 : [51-75] 

# Grupo 4 : [76-infinito]   



intervalos_p2 <- findInterval(datos_edad_ingresos$Edad, c(1, 26, 51, 76))

intervalos_p2 <- replace(intervalos_p2, intervalos_p2 == 1, "[1-25]")
intervalos_p2 <- replace(intervalos_p2, intervalos_p2 == 2, "[26-50]")
intervalos_p2 <- replace(intervalos_p2, intervalos_p2 == 3, "[51-75]")
intervalos_p2 <- replace(intervalos_p2, intervalos_p2 == 4, "[76-..]")

datos_edad_ingresos$Intervalo <- intervalos_p2

datos_p2.frame <- datos_edad_ingresos %>% 
  group_by(Intervalo) %>% summarise(Cantidad = length(Intervalo))



# Resultado:

# Intervalo     Cantidad

# [1-25]          19
# [26-50]         45
# [51-75]         22
#               ------
#                 86    

# Dado que la encuesta no se encontraron personas de edad mayor a 
# 76 años, el intervalo 4 o grupo 4 no existe. 

#---------------------------------------------------------#
# 1.2  Aplicación de la formula de muestreo estratificado #
#---------------------------------------------------------#



# Puesto que la formula para obtener el numero de muestras es :

#   muestras   =        S( Ni * Pi * Qi )
#                   -------------------------
#                   N * E + S( Ni * Pi * Qi )
#                           -----------------
#                                   N
# Donde :
#
#     N  = Tamaño total.
#     Ni = Tamaño del estrato i.
#     Pi = Peso del estrato i. Se obtiene dividiendo Ni / N .
#     Qi = Complemento de Pi. Se calcula como  (1-Pi).
#     E = Es el error. Esta dado por :

#             E   =       (error maximo permitido)^2
#                    ------------------------------------
#                     (Valor de dist. Z con (1 - a/2))^2





# 1.2.1 Numerador
#----------------

# Primeramente comencemos armando el numerador.

# Definamos entonces, el tamaño de N que es la suma de estratos: 
N <- sum(datos_p2.frame$Cantidad)

# Comenzamos a agregarles datos al frame, como el peso de
# Cada estrato (Pi):


datos_p2.frame$Peso <- ( datos_p2.frame$Cantidad  / 
                           sum(datos_p2.frame$Cantidad) 
)

# Añadimos el complemento del peso, osea 1-peso (Qi):

datos_p2.frame$Complemento <- (1 - datos_p2.frame$Peso)

# Con esto ya tenemos los datos necesarios para el numerador
# en la formula para obtener el tamaño de la muestra.

# entonces el numerador estará dado por : S( Ni * Pi * Qi )

datos_p2.frame$NiPiQi <- (datos_p2.frame$Cantidad *
                            datos_p2.frame$Peso *
                            datos_p2.frame$Complemento ) 

numerador <- sum(datos_p2.frame$NiPiQi)

# Antes de pasar a la parte del denominador lo que tenemos 
# de momento es : 

#                   N          Pi         Qi       
#   Intervalo    Cantidad    Peso    Complemento   NiPiQi

#    [1-25]         19      0.221      0.779        3.27
#    [26-50]        45      0.523      0.477        11.2 
#    [51-75]        22      0.256      0.744        4.19


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

# recordemos que el total de la población es N.

# entonces ya con esto tenemos nuestro denominador:

denominador <- (N * error) + ( sum(datos_p2.frame$NiPiQi) / N)

# Finalmente el total de las muestras que debemos tener es
# (redondeando al supremo):

muestras_edad.p2 <- ceiling(numerador / denominador )


# Como resultado nos da 69. 

# Ahora si queremos saber cuantas muestras por estratos necesitamos:

datos_p2.frame$Muestras <- (datos_p2.frame$Peso *
                              muestras_edad.p2)


# aun que falta redondear estas ultimas ya que tenemos:

# Intervalo     Muestras

# [1-25]          15.2
# [26-50]         36.1
# [51-75]         17.7
#               --------
#                  69  



#-------------------------------------------#
# 1.3  Redondeo de muestras en los estratos #
#-------------------------------------------#


# El redondeo actual está dado, por la funcion de redondeo al valor de 5

redondeo_edad.actual <- round(datos_p2.frame$Muestras)

datos_p2.frame$Muestras <- redondeo_edad.actual

# Como el redondeo actual es exacto con los datos que necesitamos, basta
# con solo ocupar este.

# Finalmente con este redondeo obtenemos valores : 

# Intervalo     Muestras

# [1-25]          15
# [26-50]         36
# [51-75]         18
#               ------
#                 69  




#-----------------------------#
# 1.4  Creación de la Muestra #
#-----------------------------#


# ahora si contamos con todos los datos casi exactos.
# por lo que podemos crear nuestra muestra: 


# con los datos que tenimos de antes, que es nuestra población

# datos_edad_ingresos = población

# Escogeremosal azar con la seed fijada

#Creamos una variable datos_p2.muestra
data.muestra.p2 <- datos_edad_ingresos %>% # De la población 
  
  group_by(Intervalo) %>% # Agrupapos por Edad
  
  nest() %>% # dejamos todos los datos en 1 fila por intervalo de edad
  
  mutate(n=datos_p2.frame$Muestras) %>% # fijamos el tamaño 
  # de cada muestra
  
  mutate(samp = map2(data, n, sample_n)) %>% # Mapeamos una función 
  # de sample segun tamaño
  
  select(Intervalo, samp) %>% # Aplicamos la función 
  
  unnest() %>% # revertimos la agrupación de filas (ahora hay n filas)
  
  as.data.frame() # volvemos a transformar a datos_p2 frame




####-------------------------------------------####
#### PARTE 2 : APLICACIÓN DEL METODO ROBUSTO   ####
####-------------------------------------------####

#DEFINICIÓN DE MÉTODOS ROBUSTOS
#Los métodos robustos son aquellos que verifican y validan la efectividad 
#de las hipótesis planteadas a partir de los datos presentes que pueden ser o no
# de caracter atípico dentro de la muestra.
#También predicen la efectividad de los  métodos paramétricos y/o
#no paramétricos para estimar distintas estimadores como la media, la mediana o la varianza de la muestra
#(En este ejercicio se debe estimar la media).

#Existen 4 tipos de metodos robustos: Estimador de Huber, Media α-recortado muestral,
#Media α-winsorizada muestral, Mediana muestral.

# En base a la lectura del curso, dentro de las bondades de R 
# existen algunos paquetes que sirven para comprobar la eficiencia de los métodos.
#El paquete "WRS2" nos facilita varias funcione para utilizar metodos robustos.

# Dadas las condiciones del problema se podria sospechar que el metodo 
# ANOVA es el indicado para comparar varias muestras. En caso de no ser así
# existe una alternativa no parametrica asociada a ANOVA, el cual sería Kruskal.

# Una alternativa para el metodo robusto de ANOVA  t1way() o t1waybt() 
# con bootstraping, dependiendo de los datos:


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

# Para ver la normalidad de los ingresos de las personas segun su edad,
# podemos generar un grafico QQ para ver como se comportan los
# datos.

# se crea una grafica de ingresos vs edad
normalidad.p2 <- ggqqplot(
  data.muestra.p2,
  x = "Ingresos",
  color = "Intervalo"
)

# se añaden todos 
normalidad.p2 <- normalidad.p2 + facet_wrap(~ Intervalo)
print(normalidad.p2)

# Podemos ver claramente que los datos que manejamos no son normales.

# incluso si vemos los datos en un grafico de cajas: 

edad.diagrama.p2 <- ggboxplot(
  data.muestra.p2,
  x = "Intervalo", y = "Ingresos",
  xlab = "Intervalo",
  color = "Intervalo",
  add = "jitter"
)
print(edad.diagrama.p2)

# podemos ver mucha presencia de outliers.

# Conciderando esto podemos decir que el metodo robusto nos viene como 
# anillo al dedo. Ya que como los datos son muy distintos o atipicos.
# La medicion cambiará si se le agrega o quita un dato.


#------------------------------#
# 2.2  Aplicación del metodo   #
#------------------------------#

# Como la alternativa robusta de ANOVA es, segun las lecturas, es 
# t1way() ya que este metodo compara diferentes medias de 1 via 
# como ANOVA


# 2.2.1 Hipotesis
#-----------------

# Antes de proceder con el metodo, veamos cuales son las hipotesis
# a contrastar : 

# H0 =  µ_ingresos_inter_1 = µ_ingresos_inter_2 =
#       µ_ingresos_inter_3

# H1 = Al menos un par de medias son distintas. 


# Dentro de este estudio tambien es importante definir un alpha.

# Como siempre manejaremos este estudio con un error del 5%. 


# 2.2.2 Aplicación
#-----------------

# entonces finalmente:


# veamos nuestra prueba:
prueba_edad.robusta.p2 <- t1way(Ingresos ~ Intervalo, data.muestra.p2)
print(prueba_edad.robusta.p2)


# Como resultado obtenemos : 


# Test statistic: F = 1.4241 
# Degrees of freedom 1: 2 
# Degrees of freedom 2: 21.88 
# p-value: 0.26217 

# Explanatory measure of effect size: 0.38


# veamos la misma prueba pero ahora con bootstrap:
prueba_edad.robusta_bootstrap.p2_v2 <- 
  t1waybt(Ingresos ~ Intervalo, data.muestra.p2)

print(prueba_edad.robusta_bootstrap.p2_v2)

# obtemos : 

# Effective number of bootstrap samples was 599.

# Test statistic: 1.4241 
# p-value: 0.29048 
# Variance explained 0.12 
# Effect size 0.346 


# 2.2.3 Analisis de Resultados
#-----------------------------


# como en ambos casos el p-valor fue mayor al alpha entregado,
# no rechazamos H0, por lo que podemos decir que el promedio
# entre los grupos si es igual o al menos similar. 

# Esto quiere decir que dado los datos recopilados en el estudio
# y segun las pruebas hechas, no hay evidencia que nos diga que el
# promedio de ingresos por persona segun su edad, es distinto
# Lo que quiere decir que no por tener más edad, tengo mas dinero 
# para gastar. 
#Además se confirma que el método ocupado para verificar la no existencia de diferencia de medias 
#entre los rangos etarios y el presupuesto que utilizaron para el evento del eclipse es el adecuado

# Indicar directorio del archivo .csv
#dir <- "C:/Users/Acer/Desktop/pep3/pep3-inferencia"

# Se hace calculo de la semilla con respecto a los dias de nacimiento de los investigadores mencionados previamente
# Semilla: d1 * d4 + d2 * d3 = 10 * 6 + 13 * 10 = 60 + 130 = 190
set.seed(190)

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

roc1 <- roc(testing$Procedencia, pred1)
roc1
plot(roc1)

roc2 <- roc(testing$Procedencia, pred2)
roc2
plot(roc2)

roc3 <- roc(testing$Procedencia, pred3)
roc3
plot(roc3)

roc.test(roc1,roc2)
roc.test(roc2,roc3)
roc.test(roc1,roc3)

# 1- Procedencia ~ Edad
# 2- Procedencia ~ Edad + Presupuesto
# 3- Procedencia ~ Edad + Presupuesto + Noches

# AUC de modelo 1 es menor a 2 y 3
# AUC de 2 y 3 son iguales