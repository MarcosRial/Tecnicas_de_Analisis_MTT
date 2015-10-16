## Universidad de Santiago de Compostela
## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización de la práctica 4

### Preliminares
# Cargamos el paquete adicional HSAUR y los datos de ejemplo
library(HSAUR)
data(weightgain)
help(weightgain)

# Comprobamos que la estructura del objeto es la correcta
str(weightgain)
# Visualizamos un encabezado y un resumen
head(weightgain)
summary(weightgain)

### Ajuste del modelo
# Comprobamos si el diseño es equilibrado
table(weightgain$source, weightgain$type)

## Pequeña disgresión
# Si quisiéramos evaluar únicamente el efecto de un factor con dos 
# niveles, podríamos hacerlo con un simple contraste de hipótesis 
# para la media de dos poblaciones
t.test(weightgain~source,data=weightgain)

# Visualizamos el efecto en un gráfico de cajas
boxplot(weightgain$weightgain~weightgain$source)
boxplot(weightgain~source,data=weightgain)

## Anova para un factor
anova1 <- aov(weightgain~source, data=weightgain) 
summary(anova1)

# Podemos comprobar la normalidad del conjunto de observaciones:
shapiro.test(weightgain$weightgain)
# O de cada uno de los grupos por separado:
tapply(weightgain$weightgain, weightgain$source, shapiro.test)
# Y la igualdad de varianzas:
var.test(weightgain~source, data=weightgain)
bartlett.test(weightgain~source, data=weightgain)

oneway.test(weightgain~source, data = weightgain)

# Comprobación gráfica de normalidad
qqnorm(weightgain$weightgain[weightgain$source=="Beef"])
qqline(weightgain$weightgain[weightgain$source=="Beef"])
qqnorm(weightgain$weightgain[weightgain$source=="Cereal"])
qqline(weightgain$weightgain[weightgain$source=="Cereal"])


# Si no fuera posible aceptar el supuesto de homocedasticidad
oneway.test(weightgain~source,data=weightgain)

## Anova para dos factores

# Análisis preliminares
tapply(weightgain$weightgain,list(weightgain$source,weightgain$type),mean)
tapply(weightgain$weightgain,list(weightgain$source,weightgain$type),sd)
boxplot(weightgain~source+type,data=weightgain)
plot.design(weightgain)


# Ajuste del modelo (sin interacción)
anova2 <- aov(weightgain~source+type, data=weightgain)
print(anova2)
summary(anova2)


# Ajuste de un modelo con interacción
anova2b <- aov(weightgain~source*type, data=weightgain)
print(anova2b)
summary(anova2b)

# Exploramos gráficamente los efectos de interacción
interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain)
interaction.plot(weightgain$source, weightgain$type, weightgain$weightgain)

# Test de comparaciones múltiples
TukeyHSD(anova2b, conf.level=.95)
