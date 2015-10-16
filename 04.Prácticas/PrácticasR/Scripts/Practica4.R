## Universidad de Santiago de Compostela
## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización de la práctica 4

### Preliminares
# Instalamos el paquete adicional HSAUR
install.packages("HSAUR")

# Una vez instalado, procedemos a cargarlo
library(HSAUR)

# Cargamos los datos de ejmplo 'clouds'
data(clouds)

# Podemos obtener información sobre los datos con
help(clouds) # O, alternativamente, '?clouds'

# Comprobamos la estructura del objeto 'clouds'
str(clouds)

### Ajuste de los modelos
## Modelo 1: regresión lineal simple

# Gráfico de dispersión
plot(rainfall~cloudcover, data=clouds, pch=19, col="grey")
# El mismo gráfico también se puede generar con:
# plot(clouds$cloudcover,clouds$rainfall) 

# Utilizamos la variable 'seeding' para separar en el gráfico la lluvia artificial
plot(rainfall~cloudcover, data=clouds, pch=19, col=seeding)
legend("topright", legend=levels(clouds$seeding), 
       col=1:2, 
       pch=19, 
       title="seeding")

# Seleccionamos los casos en los que la lluvia fue provocada
artif <- which(clouds$seeding=="yes")

# Ajustamos un primer modelo para relacionar la precipitación con la cubierta de nubes
# (excluimos los casos en los que la lluvia fue provocada)
modelo1 <- lm(rainfall~cloudcover, data=clouds, subset=-artif)
# Invocar el nombre del modelo una vez ajustado nos proporciona información básica:
modelo1
# Y utilizar el comando summary nos proporciona información detallada
summary(modelo1)

# Podemos representar la recta definida por el modelo con la siguiente expresión:
plot(rainfall~cloudcover, data=clouds, 
     pch=19, col="grey", subset=-artif)
text(x=clouds$cloudcover[-artif], y=clouds$rainfall[-artif], 
     labels=rownames(clouds[-artif,]), pos=2)
abline(modelo1)

# Comprobamos el cumplimiento de los supuestos de partida de la regresión
plot(modelo1)

## Modelo 1b
# Ajustamos un modelo equivalente pero excluyendo la observación número 1
modelo1b <- lm(rainfall~cloudcover, data=clouds, subset=-c(1,artif))
summary(modelo1b)
plot(modelo1b)

## Modelo 2
# Modelo robusto mediante el M-estimador de Huber
library(MASS)
modelo2 <- rlm(rainfall~cloudcover, data=clouds, subset=-artif)
summary(modelo2)

### Representación gráfica de los tres modelos
plot(rainfall~cloudcover, data=clouds, subset=-artif, pch=19, col="grey")
abline(modelo1, col="red")
abline(modelo1b, col="red", lty=2)
abline(modelo2, col="blue")
legend("topleft", legend=c("modelo1","modelo1b","modelo2"),
       col=c("red","red","blue"), lty=c(1,2,1), bty="n")
