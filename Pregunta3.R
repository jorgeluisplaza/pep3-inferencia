
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

# Se utiliza muestreo simple aleatorio para la obtención de la muestra
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
