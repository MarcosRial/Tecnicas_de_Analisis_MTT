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

qqnorm(datos$total.minutos)
qqline(datos$total.minutos)
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

#### Contraste de hipótesis para tiempo y sexo ####
# Contraste de hipótesis
t.test(datos$total.minutos[datos$sexo == "M"])
t.test(datos$total.minutos[datos$sexo == "F"])

t.test(total.minutos~sexo, data = datos)
boxplot(datos$total.minutos ~ datos$sexo,
        col = c("lightgreen", "lightblue"),
        names = c("Mujeres", "Hombres"),
        ylab = "Total minutos")

#### ANOVA de dos factores ####
# Por separado
anova1 <- aov(total.minutos~sexo+categoria, data = datos)
anova1
summary(anova1)

# Juntas
anova2 <- aov(total.minutos~sexo*categoria, data = datos)
anova2
summary(anova2)

# Interacción
anova3 <- aov(total.minutos~sexo:categoria, data = datos)
anova3
summary(anova3)

# Gráficas
interaction.plot(datos$sexo, datos$categoria, datos$total.minutos,
                 col= c("darkgreen", "blue", "darkred", "grey50"),
                 ylab = "Tiempo medio (minutos)",
                 xlab = "Sexo",
                 trace.label = "Categoría")
interaction.plot(datos$categoria, datos$sexo, datos$total.minutos,
                 col = c("darkgreen", "blue"),
                 ylab = "Tiempo medio (minutos)",
                 xlab = "Categoría",
                 trace.label = "Sexo",
                 cex.axis = 0.8)

#### Análisis de homocedasticidad ####
# Variable sexo
var.test(total.minutos ~ sexo, data = datos)
# Variable categoria
bartlett.test(total.minutos ~ categoria, data = datos)

#### Exportado de gráficos ####
png("./Graficos/Interaccion2.png", #modificar a conveniencia
    width=10, height=10/1.5, units="cm",
    res=300, pointsize=8)

dev.off()