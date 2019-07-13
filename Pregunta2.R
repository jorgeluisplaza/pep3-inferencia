


#### Pregunta 2 -> Comparación de mas de 2 medias

# En la actividad realizada en la región de Coquimbo se recabaron datos respecto a como los 
# turistas se iban a alimentar durante su estadia. Las opciones que se les dieron a los
# turistas para escoger fueron:


#   Restaurant :    Los tutistas preferian ir a comer a Resuarants
#   Cocina :        Los tutistas prefieren cocinarse ellos mismos.
#   Comida Rapida:  Los turistas prefieren ir a comer a locales de comida rápida.
#   Otro:           Los tutistas prefieren 2 opciones para alimentarse.


# De los datos obtenidos se quiere saber si existe alguna diferencia en el promedio de
# gasto diario de las personas que comieron en en estas diferentes opciones fue distinto.
# Esto quiere decir que el promedio de ingresos de las personas que comen en restaurant
# es mayor al promedio de las personas que comen en otro lugar ( por ejemplo ).


# ----------------------------------------------------
# Hipotesis
# ----------------------------------------------------

# entonces nuestra hipotesis nula es :

# H0 : μ.ingresos_Restaurant = μ.ingresos_Cocina = μ.ingresos_Comida_Rapida = μ.ingresos_otro
# H1 : Al menos un par de medias es distintas.

# Dada las caracteristicas de este estudio, la mejor alternativa para esto
# es utilizar un procedimiento ANOVA.


# ----------------------------------------------------
# Condiciones
# ----------------------------------------------------

# Primero veamos si se cumplen las condiciones



# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes y obtenidas aleatoriamente
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares

# ----------------------------------------------------
# Verificacion
# ----------------------------------------------------


### 1:

# Como nos intersa saber del ingreso, este esta medido en pesos chilenos
# por lo que si tomamos 100.000 pesos chilenos y lo dividimos en 2
# tendromos 50.000 pesos y ademas en la mitad de 200.000 por lo que 
# si se cumple esta condición.

### 2:

# Como los datos fueron optenidos de personas tomadas al azar en disintas
# partes de La Serena, se cumple la segunda condición de independencia.

### 3:

# Para ver la normalidad de los ingresos de las personas segun donde van 
# a comer, podemso generar un grafico QQ para ver como se comportan los
# datos.

#################################
#        Captura de Datos       #
#################################

data <- read.csv("eclipse2019.csv", header = TRUE)
Personas <- factor(1:nrow(data))


data.alimentos <- data.frame(
  Alimento = factor(data[["Alimentacion"]]),
  Ingresos = data[["Presupuesto"]]
)

normalidad <- ggqqplot(
  data.alimentos,
  x = "Ingresos",
  color = "Alimento"
)

normalidad <- normalidad + facet_wrap(~ Alimento)
print(normalidad)

# Claramente los datos no cumplen con la normalidad por lo que se descarta
# ocupar ANOVA para tratar esta prueba.

#--------------------------------------------------------
#--------------------------------------------------------

# Una alternativa es ocupar un metodo no parametrico
# Kruskall-Wallis




# Instalación del paquete "Ez"
# install.packages("ez")

# Importación de libreria "ez"
library(ez)



