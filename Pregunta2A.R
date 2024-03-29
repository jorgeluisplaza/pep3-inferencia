
# install.packages("ez") # InstalaciÃ³nn del paquete "Ez"


# install.packages("tidyverse") # InstalaciÃ³nn del paquete "tidyverse"


# install.packages("WRS2") # InstalaciÃ³n del paquete "WRS2"

# ImportaciÃ³n de modulo "ez"
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
# Fernando Guzmán, presidente de la Cámara de Comercio de Coquimbo, coincidió y afirmo que: 
# “Aún no hemos sacado todas las cuentas, pero hasta el momento hemos visto que el mayor flujo de turistas fue domingo, lunes y martes. 
# Nos vimos altamente beneficiados por la ocupación hotelera, ya que hostales, hoteles y residenciales estuvieron copados. 
# Los presupuestos fueron satisfechos también por los restaurantes y locales de comida”, dijo.
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
# El muestreo es una herramienta de investigaciÃ³n cientÃ­fica, cuya funciÃ³n
# bÃ¡sica es determinar que parte de una poblaciÃ³n debe examinarse, con la finalidad de hacer
# inferencias sobre dicha poblaciÃ³n. El muestreo estratificado es una tÃ©cnica de muestreo 
# probabilÃ­stico en donde el investigador divide a toda la poblaciÃ³n en diferentes subgrupos 
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

