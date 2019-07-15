# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara

# Los datos fueron recolectados en terreno por los estudiantes
# que realizan el analisis. 
# Se basa en una encuesta a las personas que asistieron a ver 
# el eclipse total sol de la Region de Coquimbo en Chile durante
# el 2 de Julio de 2019, se estudian las siguientes variables:

## Link de descarga: https://docs.google.com/spreadsheets/d/15mG2ATv2ArBVioZ8NSSuixG7Fr_cX8iw_0K3thaGz_M/edit?usp=sharing #

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

# Se obtienen las procedencia(extranjero o Chileno) y la localidades(vicuña, la higuera y la serena) de la tabla de datos
tabla <- data.frame(Procedencia=datos.todos$Procedencia,Localidad=datos.todos$Localidad)


#(B) 

# Definiendo valores razonables (y bien justificados) para los diferentes factores para este estudio, el equipo
# ha de determinar el tamaño de muestra necesitado para realizar esta comparación estadística utilizando
# muestreo sistemático.



# El tamaño de la muestra corresponde al número mínimo necesario para estimar el parámetro poblacional 
# con la restricción que la diferencia entre el estadístico y el parámetro sea menor que una cantidad 
# condicionalmente aceptada. Al utilizar Muestreo Sistematico  debemos utilizar la siguiente formula:

P= 0.5 # = Probabilidad de Ocurrencia del Fenómeno Estudiado
Q = 0.5 # = Probabilidad de que no Ocurra el Fenómeno (q = 1 – p)
N = 86 # numero de la población
e= 0.05 # Maximo error permitido 5 %
D = (e^2)/4
nMuestras = (N*P*Q)/((N-1)*D + P*Q)
nMuestras <- ceiling(nMuestras) # aproximar hacia arriba
cat("Numero de muestras a utilizar :")
cat(nMuestras)

# Formula obtenida:  VI Muestreo Sistemático, Dr. Jesús Mellado Bosque
# http://www.uaaan.mx/~jmelbos/muestreo/muapu4.pdf

#(D)
##### Semillas ####### 
# Dias de nacimiento
# Matias Paredes: d1 = 10
# Vicente Rivera: d2 = 10
# Nicolas Alarcon: d3 = 6
# Sandra Hernandez: d4 = 26


# Semilla: d1 * d2 + d3 * d4 = 10 * 10 + 6 * 26 = 256
set.seed(256)


### EXPLICAR QUE HACER Y COMO FUNCIONA #### 
#(C)
# Se utiliza muestreo sistematico para el calculo de la muestra
n.sys <- nMuestras    # numero de muestras # 
index <- sys.sample(N=nrow(tabla), n=n.sys) ##  ***************# 
muestra <- tabla[c(index), ]##  ***************# 
frec <- 1:nrow(muestra)##  ***************# 
p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = muestra, FUN = length)

### PLOTEAR LOS RESULTADOS ### 


# Se calculan la cantidad de personas en las tres localidades 
vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]

# Se guarda en un data.frame
table.p1.1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)

# Se definen el nombre de las filas 
rownames(table.p1.1) <- c("Chileno", "Extranjero")

# El equipo debe usar bootstrapping para responder su pregunta de investigación con la muestra obtenida.# 


# Se define estadistico para el bootstraping
foo <- function(data, indices){
  frec <- 1:nrow(data)
  data <-  data[c(indices), ]
  p1.1 <- aggregate(frec ~ Procedencia + Localidad, data = data, FUN = length)
  vicuña <- p1.1[c(which(p1.1$Localidad == "Vicuña")), "frec"]
  higuera <- p1.1[c(which(p1.1$Localidad == "La Higuera")), "frec"]
  serena <- p1.1[c(which(p1.1$Localidad == "La Serena")), "frec"]
  table.p1.1 <- data.frame(Vicuña=vicuña, "La Higuera"=higuera, "La Serena"=serena)
  rownames(table.p1.1) <- c("Chileno", "Extranjero")
  chi <- chisq.test(table.p1.1)
  return(chi$statistic)
}

# Se calcula el bootstraping sobre la cantidad de repeticion n.perm
n.perm <- 1000
bootobj <- boot(muestra, foo, R = n.perm)
distribucion <- bootobj$t



####################################
##### PROCEDIMIENTO ################
####################################


# Se tiene una "tabla de dos vías"  que registra las
# frecuencias observadas para las posibles combinaciones de dos
# variables categóricas.  Para esto existe un procedimiento χ^2, que se le conoce de forma 
# Prueba χ^2 de Independencia. En donde hay dos factores ("Lugara a Visitar" y "Procedencia")
# que se miden en una misma población

cat("\n")

cat("HO: el lugar  a visitar no depende de la precedencia de la persona.")
cat("HA: la procedencia incide en el lugar a visitar  \n")

# Se define un alpha de 0.05
alpha <- 0.05  es decir 95% confianza.


# Ahora con la función chisq.test de R:


observado <- chisq.test(table.p1.1)$statistic
count <- sum(distribucion > observado)
p.value <- (count + 1)/(n.perm + 1)
p.95 <- (1 - alpha)*n.perm
distribucion <- sort(distribucion)
limit <- distribucion[p.95]


# Se grafica y se mmuestran los valores observados y obtenidos
hist(distribucion, breaks = 25)
abline(v=observado, col="blue")
abline(v=limit, col="red")

# ************* FIN PREGUNTA 1 FORMA A *********************** #