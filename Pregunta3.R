
library(stats)
library(ggpubr)

# Indicar directorio del archivo .csv
dir <- ""

# Indicar nombre del archivo
basename <- "eclipse2019.csv"

# Se lee el archivo   
#file <- file.path(basename)

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
proporcion <- 0.8
nmax <- min( length(extranjeros), length(chilenos))*proporcion

# Se utiliza muestreo simple aleatorio para la obtenciÃ³n de la muestra
muestra.chilenos <- tabla[ sample( chilenos, nmax), ]
muestra.extranjeros <- tabla[ sample( extranjeros, nmax), ]

muestra <- rbind(muestra.chilenos, muestra.extranjeros)

#box.noches <- ggboxplot(
#  data = muestra,
#  x = "Procedencia", y = "Valor", 
#  color = "Noches", legend = "none"
#)
#pbf <- facet(box.noches, facet.by = "Noches", scales = "free")
#plot(box.noches)


nulo <- glm(
  Procedencia ~ 1,
  family = binomial(link = "logit"),
  data = muestra
)

modelo <- glm(
  Procedencia ~ Edad, 
  family=binomial(link='logit'), 
  data=muestra, 
  na.action = na.omit
)

comparacion <- anova(nulo, modelo, test = "LRT")
print(comparacion)

nuevo <- update(modelo, . ~ . + Noches)

comparacion <- anova(modelo, nuevo, test = "LRT")
print(comparacion)

nuevo2 <- update(modelo, . ~ . + Presupuesto)

comparacion <- anova(modelo, nuevo2, test = "LRT")
print(comparacion)

#predicted <- predict(modelo, muestra, type="response")  # predicted scores

#plot(predicted)



# Comentarios de aqui hacia abajo siguen el mismo esquema del script del profe (hasta ahora ocurren
# las mismas situaciones del script del profe)

# Se queda con el modelo Procedencia ~ Edad + Presupuesto tras realizar la comparacion

modelo <- nuevo2


#Comentarios de aqui hacia abajo siguen el mismo esquema del script del profe

# Se queda con el modelo Procedencia ~ Edad + Presupuesto
# Revision de casos sospechosos (copia barata del script del profe)
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
sospechosos3 <- which(output[["leverage"]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)
cat("\n\n")
cat("Residuales con levarage fuera de rango (> ")
cat(round(leverage.promedio, 3), ")", "\n", sep = "")
cat("--------------------------------------\n")
print(rownames(subdatos[sospechosos3, ]))

# Tambien podriamos revisar DFBeta, que deberia ser < 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------\n")
print(rownames(subdatos[sospechosos4, ]))

# Puede verse que los casos sospechosos mas o menos se repiten con cada
# estadistica. Revisemos estos casos.

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
# Sacarlos del modelo o dejarlos

# No se que hacer con esos casos


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



# 2) Independencia del error (residuales)
# 
# Es se traduce a que no ha de existir autocorrelación en los términos
# residuales. Esto puede probarse con una prueba estadística específica
# conocida con el nombre de sus autores: Durbin–Watson test, que
# verifica si dos residuales adyacentes (un retardo) están
# correlacionados.
# [J Durbin, GS Watson (1950). Testing for Serial Correlation in Least
# Squares Regression, I". Biometrika. 37(3-4):409-428;
# Durbin, GS Watson (1951). Testing for Serial Correlation in Least
# Squares Regression, II". Biometrika. 38(1-2):159-179]

# En R, es fácil revisar más retardos, por ejemplo hasta 5 retardos:

library(car)
cat("\n\n")
cat("Prueba de Durbin–Watson para autocorrelaciones entre errores\n")
cat("------------------------------------------------------------\n")
print(durbinWatsonTest(modelo, max.lag = 5))

# En este caso, el test indica autocorrelaciones significativas
#   0 <= p-value < 2  => correlaciones positivas
#   2 < p-value <= 4  => correlaciones negativa
#   p-value = 2       => no hay correlacion
#
# Asi, debemos dudar del modelo obtenido.
# 
# En todo caso, debe tenerse cuidado con la Prueba de Durbin–Watson,
# puesto que depende del orden de los datos, por lo que reordenando
# se podria tener valores distintos. Aunque en este caso, es poco
# probable que cambie la conclusion de que existe autocorrelacion.

