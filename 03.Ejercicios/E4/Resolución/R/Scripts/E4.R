## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de Análisis Cuantitativas y Cualitativas
## Script de R para la realización del efercicio 4 de evaluación
## 13/11/2015
## Marcos Rial Docampo

#### Inicio del ejercicio ####
# Cargamos los datos
datos <- read.csv("Datos/MuestraTiempos.csv", sep = ";")

#### Análisis de la variable total.minutos ####
# Para todo el conjunto de datos de la variable
shapiro.test(datos$total.minutos)
boxplot(datos$total.minutos, col = "grey")
abline(h = seq(30,70,10), col = "grey")

# Dividiendo por grupos
tapply(datos$total.minutos, datos$categoria.subcategoria, shapiro.test)
tapply(datos$total.minutos, datos$categoria, shapiro.test)
tapply(datos$total.minutos, datos$sexo, shapiro.test)

# Análisis gráfico de cuartiles
qqnorm(datos$total.minutos[datos$categoria =="INFANT-CADETE"],
       pch = 19, col = "dodgerblue", main = "Infantil-Cadete",
       xlab = "Cuartiles teóricos", ylab = "Cuartiles de la muestra")
qqline(datos$total.minutos[datos$categoria =="INFANT-CADETE"],
       col = "grey50")

qqnorm(datos$total.minutos[datos$categoria =="JUNIOR"],
       pch = 19, col = "dodgerblue", main = "Junior",
       xlab = "Cuartiles teóricos", ylab = "Cuartiles de la muestra")
qqline(datos$total.minutos[datos$categoria =="JUNIOR"],
       col = "grey50")

qqnorm(datos$total.minutos[datos$categoria =="SENIOR"],
       pch = 19, col = "dodgerblue", main = "Senior",
       xlab = "Cuartiles teóricos", ylab = "Cuartiles de la muestra")
qqline(datos$total.minutos[datos$categoria =="SENIOR"],
       col = "grey50")

qqnorm(datos$total.minutos[datos$categoria =="VETERANO"],
       pch = 19, col = "dodgerblue", main = "Veteranos",
       xlab = "Cuartiles teóricos", ylab = "Cuartiles de la muestra")
qqline(datos$total.minutos[datos$categoria =="VETERANO"],
       col = "grey50")

#### Exportado de gráficos ####
png("./Graficas/CuartVeteranos.png", #modificar a conveniencia
    width=10, height=10/1.5, units="cm",
    res=300, pointsize=8)

dev.off()
