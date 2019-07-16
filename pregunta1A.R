# PEP 3 Inferencia y Modelos Estadisticos
# Profesor: Jose Luis Jara

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
# con la restricción que la diferencia entre el estadístico y el parámetro sea menor que una cantidad 
# condicionalmente aceptada. Al utilizar Muestreo Sistematico  debemos utilizar la siguiente formula:

P= 0.5 # = Probabilidad de Ocurrencia del Fenómeno Estudiado
Q = 0.5 # = Probabilidad de que no Ocurra el Fenómeno (q = 1 – p)
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
##### Semillas ######

# Dias de nacimiento
# Matias Paredes: d1 = 10
# Vicente Rivera: d2 = 10
# Nicolas Alarcon: d3 = 6
# Sandra Hernandez: d4 = 26

# Semilla: d1 * d2 + d3 * d4 = 10 * 10 + 6 * 26 = 256
set.seed(256)

#(C)
# Se utiliza muestreo sistematico para el calculo de la muestra
# El muestreo sistemático define intervalos fijos de muestreo definido por
# k = N/n.sys (división de la población en n.sys partes iguales)
# sys.sample selecciona una muestra al azar dentro del primer intervalo [1, k]
# Luego retorna un arreglo con los índices, desde la primera muestra i,
# luego i + k, i + 2k, ... i + (n.sys - 1)*k resultando en n.sys muestras (indices)
n.sys <- nMuestras    # numero de muestras # 
index <- sys.sample(N=nrow(tabla), n=n.sys)
muestra <- tabla[c(index), ]
frec <- 1:nrow(muestra)

####################################
#####PROCEDIMIENTO ################
####################################

# Se tiene una "tabla de dos vías"  que registra las
# frecuencias observadas para las posibles combinaciones de dos
# variables categóricas.  Para esto existe un procedimiento χ^2, que se le conoce de forma 
# Prueba χ^2 de Independencia. En donde hay dos factores ("Lugara a Visitar" y "Procedencia")
# que se miden en una misma población

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
# 
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

cat("\n")

cat("HO: el lugar  a visitar no depende de la precedencia de la persona.")
cat("HA: la procedencia incide en el lugar a visitar  \n")

# Se define un alpha de 0.05
# es decir 95% confianza.
alpha <- 0.05 

# Ahora con la función chisq.test de R, se obtiene el estadístico observado
observado <- chisq.test(table.p1)$statistic

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

############# P-valor y conclusiones ###################

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

