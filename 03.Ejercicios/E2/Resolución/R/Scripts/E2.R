## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de Análisis Cuantitativas y Cualitativas
## Script de R para la realización del efercicio 2 de evaluación
## 26/10/2015
## Marcos Rial Docampo

# Cargamos la tabla de datos
datos <- read.csv("Datos/DatosExerc2.csv")

# Comprobacion de estructura y previsualizacion
str(datos)
summary(datos)

#### Relaciones ####
# Relación entre el abandono de superficie agrícola y la densidad de población
plot(pop.dens~abandon.uaa, data = datos,
     col = "darkgoldenrod3", bg = "azure4", pch = 21,
     ylab = "densidad de pob. (hab./km2)",
     xlab = "Abandono de sup. agrícola",
     main = "Relación 1")

# Relación entre el abandono de superficie agrícola y la altitud
plot(elevation~abandon.uaa, data = datos,
     col = "darkgoldenrod3", bg = "sienna", pch = 21,
     ylab = "Elevación (m)",
     xlab = "Abandono de sup. agrícola",
     main = "Relación 2")

#### Estudio de correlación entre variables ####
# H0 = las variables no están correlacionadas
# Abandono vs densidad de población
cor.test(datos$abandon.uaa, datos$pop.dens, alternative = "greater",
         method = "pearson", conf.level = 0.95)
# Abandono vs elevación
cor.test(datos$abandon.uaa, datos$elevation, alternative = "greater",
         method = "pearson", conf.level = 0.95)

#### Ajuste lineal múltiple ####
modelom <- lm(datos)

#Comprobación de resultados
modelom
summary(modelom)

#### Ajuste lineal simple ####
modelo1 <- lm(pop.dens~abandon.uaa, data = datos)
modelo2 <- lm(elevation~abandon.uaa, data = datos)

# Comprobación de resultados
modelo1
summary(modelo1)
modelo2
summary(modelo2)

# Graficado de los modelos
# Modelo 1
plot(pop.dens~abandon.uaa, data = datos,
     col = "darkgoldenrod3", bg = "azure4", pch = 21,
     ylab = "densidad de pob. (hab./km2)",
     xlab = "Abandono de sup. agrícola",
     main = "Modelo 1")
abline(modelo1, col = "grey")

#Modelo 2
plot(elevation~abandon.uaa, data = datos,
     col = "darkgoldenrod3", bg = "sienna", pch = 21,
     ylab = "Elevación (m)",
     xlab = "Abandono de sup. agrícola",
     main = "Modelo 2")
abline(modelo2, col = "grey")

#### Exportado de gráficos en png ####
png ("Graficos/Supuestos.png", #cambiar nombre según convenga
     width=10, height=10/1.5, units="cm",
     res=300, pointsize=8)

dev.off()
