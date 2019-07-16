# install.packages("ez") # Instalaciónn del paquete "Ez"


# install.packages("tidyverse") # Instalaciónn del paquete "tidyverse"


# install.packages("WRS2") # Instalación del paquete "WRS2"

# Importación de modulo "ez"
library(ez)

# Modulos para graficos
library(ggplot2)
library(magrittr)
library(ggpubr)

# Modulo de utilidad
library(dplyr)
library(tidyr)
library(purrr)

# Modulo de metodo robusto
library(WRS2)



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
  
  group_by(Intervalo) %>% # Agrupapos por alimentos
  
  nest() %>% # dejamos todos los datos en 1 fila por alimento
  
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
print(alimentos.diagrama.p2)

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


