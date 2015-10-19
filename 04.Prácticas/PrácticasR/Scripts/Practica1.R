## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización de la práctica 1. Introducción a R y RStudio.
## 16/10/2015
## Marcos Rial Docampo

#Una vez tomados los datos en clase (una variable numérica y otra categórica) los importamos en el entorno de trabajo
datos1 <- read.csv("./Datos/DatosClase.csv")

#Solicitamos el listado del espacio de trabajo
ls()

#Y comprobamos la estructura del archivo
str(datos1) #variable categórica "frame" y numérica "int"
summary(datos1)

#Formas de acceder a los datos de la tabla
datos1$altura
class(datos1$altura)
mean(datos1$altura)
median(datos1$altura)
quantile(datos1$altura)
max(datos1$altura)
min(datos1$altura)
sum(datos1$altura)
sd(datos1$altura)
var(datos1$altura)
summary(datos1$altura)

#Igual con la variable categórica
class(datos1$sexo)
levels(datos1$sexo)
table(datos1$sexo)

#Para obtener el valor de la segunda observación de la variable "sexo"
datos1$sexo[2]

#Podemos emplear expresiones lógicas
datos1$altura >=170
which(datos1$altura >=170)
datos1$altura[which(datos1$altura >=170)]

#Probamos el uso de los subíndices para visualizar datos de la tabla
datos1[1,] #primera fila
datos1[,1] #primera columna
datos1[3,2] #datos de la tercera fila segunda columna
datos1[1:3,] #tres primeras filas
datos1[c(2,5),] #filas segunda y quinta

#Representación gráfica de los datos
#Mediante un histograma
hist(datos1$altura,
     col = "lightgrey",
     xlab = "Altura (cm)",
     ylab = "Frecuencia",
     main = "Histograma de Altura")

#Mediante un diagrama de cajas
boxplot(datos1$altura,
        col = "lightgrey",
        ylab = "Altura (cm)")

#Igual para la variable categórica
frecuencias <- table(datos1$sexo)
barplot(frecuencias)
boxplot(altura~sexo, data = datos1,
        col = "lightgrey")