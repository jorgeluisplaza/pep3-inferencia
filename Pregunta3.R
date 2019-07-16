library(stats)
library(ggpubr)

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

muestra.chilenos <- tabla[ sample( chilenos, nmax), ]
muestra.extranjeros <- tabla[ sample( extranjeros, nmax), ]



nulo <- glm(
  Procedencia ~ 1,
  family = binomial(link = "logit"),
  data = muestra
)
print(summary(nulo))

modelo.edad <- glm(
  Procedencia ~ Edad, 
  family=binomial(link='logit'), 
  data=muestra, 
  na.action = na.omit
)

print(summary(modelo))

comparacion <- anova(nulo, modelo.edad, test = "LRT")
print(comparacion)

modelo.edad.noches <- update(modelo.edad, . ~ . + Noches)

comparacion <- anova(modelo.edad, modelo.edad.noches, test = "LRT")
print(comparacion)

modelo.edad.presupuesto <- update(modelo.edad, . ~ . + Presupuesto)

comparacion <- anova(modelo.edad, modelo.edad.presupuesto, test = "LRT")
print(comparacion)

modelo <- modelo.edad.presupuesto

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

# Para este caso, los casos atípicos fueron extranjeros con altos presupuestos
# o chilenos de mucha edad.


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

cat("\n")
cat("Factor de inflacion de la varianza medio\n")
cat("----------------------------------------\n")
print(round(mean(vifs), 1))

tols <- 1/vifs
cat("\n")
cat("Tolerancia\n")
cat("----------\n")
print(round(tols, 2))


# Segun los resultados encontrados:
#   VIF = 1           => bajo el umbral propuesto (VIF < 10)
#   VIF promedio = 1  => en el limite, por lo que puede existir sesgo
#   Tolerancia = 0.97    => sobre los valores propuestos (T > 0.4 siendo exigentes)

# Por lo tanto se concluye que no se esta presente frente a multicolinealidad
# (aunque puede existir sesgo)

######################################################

# Evaluación del modelo logístico usando predicciones
# Se crean los modelos anteriores separando la muestra 
# en datos de entramiento y prueba

library(caret)
#Crear particion de muestra con proporcion:

# - 70% datos de entrenamiento
# - 30% datos de prueba
Train <- createDataPartition(muestra$Procedencia, p = 0.7, list=FALSE)

training <- muestra[ Train, ]
testing <- muestra[ -Train, ]

# Modelo número 1: Procedencia ~ Edad
modelo.glm.1 <- glm(Procedencia ~ Edad, data = training, family="binomial"(link="logit"))

# Modelo número 2: Procedencia ~ Edad + Noches
modelo.glm.2 <- glm(Procedencia ~ Edad + Noches, data = training, family="binomial"(link="logit"))

# Modelo número 3: Procedencia ~ Edad + Presupuesto
modelo.glm.3 <- glm(Procedencia ~ Edad + Presupuesto, data = training, family="binomial"(link="logit"))

# Se obtiene la probabilidad predicha por el modelo.
# Retorna lista con las probabilidades del 
pred1 <- predict(modelo.glm.1, newdata=testing, type="response")
pred2 <- predict(modelo.glm.2, newdata=testing, type="response")
pred3 <- predict(modelo.glm.3, newdata=testing, type="response")

# Si la predicción da menor que 50%, se responde "Chileno" (0)
# de lo contrario "Extranjero" (1)
responses.glm.1 <- ifelse(pred1 < 0.5, "Chileno", "Extranjero")
responses.glm.2 <- ifelse(pred2 < 0.5, "Chileno", "Extranjero")
responses.glm.3 <- ifelse(pred3 < 0.5, "Chileno", "Extranjero")

# Se realiza una tabla de contingencia para las predicciones. 
# Los valores de la matríz que no coinciden sus nombres
# indican errores de predicción. 

table(responses.glm.1, testing$Procedencia)
table(responses.glm.2, testing$Procedencia)
table(responses.glm.3, testing$Procedencia)

# Se calcula el porcentaje de aciertos del modelo para el conjunto
# de pruebas. El modelo elegido fue el tercero (Edad + Presupuesto)
# que obtuvo un 91.7% de acierto.
# En este caso es muy pequeño (12 casos), por lo que no resulta
# una medida confiable. 
mean(responses.glm.1 == testing$Procedencia)
mean(responses.glm.2 == testing$Procedencia)
mean(responses.glm.3 == testing$Procedencia)



# Likelihood Ratio Test
# Lo mismo que esta mas arriba con los casos sospechosos, VIF y condiciones (debiese ser lo mismo)

# Modelo automático de R. A partir del modelo nulo (Constante)
# Agregar variables predictoras de manera ascendente por pasos.

#Es la misma muestra pero incluyendo todas las columnas 
training.modelo.R <- muestra.completa[ Train, ]
testing.modelo.R <- muestra.completa[ -Train, ]

modelo.completo <- glm(Procedencia ~ 1, data=training.modelo.R, na.action="na.omit", family=binomial(link="logit"))

modelo.auto <- step(
  modelo.completo,
  direction = "forward",
  scope=list(upper=~.,lower=~1),
  trace = TRUE,
  steps = 3
)


library(pROC)


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