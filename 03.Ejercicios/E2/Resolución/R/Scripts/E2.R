## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de Análisis Cuantitativas y Cualitativas
## Script de R para la realización del efercicio 2 de evaluación
## 26/10/2015
## Marcos Rial Docampo

#### Inicio del ejercicio ####
# Cargamos la tabla de datos
datos <- read.csv("Datos/DatosExerc2.csv")

# Comprobacion de estructura y previsualizacion
str(datos)
summary(datos)

#### Relaciones ####
# Relación entre el abandono de superficie agrícola y la densidad de población
plot(abandon.uaa~pop.dens, data = datos,
     col = "darkgoldenrod3", bg = "azure4", pch = 21,
     xlab = "densidad de pob. (hab./km2)",
     ylab = "Abandono de sup. agrícola",
     main = "Relación 1")

# Relación entre el abandono de superficie agrícola y la altitud
plot(abandon.uaa~elevation, data = datos,
     col = "darkgoldenrod3", bg = "sienna", pch = 21,
     xlab = "Elevación (msnm)",
     ylab = "Abandono de sup. agrícola",
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
modelom <- lm(abandon.uaa~pop.dens+elevation, data = datos)

#Comprobación de resultados
modelom
summary(modelom)

#### Ajuste lineal simple ####
modelo1 <- lm(abandon.uaa~pop.dens, data = datos)
modelo2 <- lm(abandon.uaa~elevation, data = datos)

# Comprobación de resultados
modelo1
summary(modelo1)
modelo2
summary(modelo2)

# Graficado de los modelos
# Modelo 1
plot(abandon.uaa~pop.dens, data = datos,
     col = "darkgoldenrod3", bg = "azure4", pch = 21,
     xlab = "densidad de pob. (hab./km2)",
     ylab = "Abandono de sup. agrícola",
     main = "Modelo 1")
abline(modelo1, col = "grey")

#Modelo 2
plot(abandon.uaa~elevation, data = datos,
     col = "darkgoldenrod3", bg = "sienna", pch = 21,
     xlab = "Elevación (msnm)",
     ylab = "Abandono de sup. agrícola",
     main = "Modelo 2")
abline(modelo2, col = "grey")
text(300,0.6, labels = "y=-0.0896+0.0006x", col = "grey")

#### Supuestos de partida de los modelos de regresión ####
plot(modelom, sub.caption = NA,
     caption = c("Residuos vs Ajuste", "Diagrama de cuantiles de los residuos", "Variabilidad de los residuos al ajuste", "Residuos vs apalancamiento"))
plot(modelo2)

#### Exportado de gráficos en png ####
png ("Graficos/Supuesto1m.png", #cambiar nombre según convenga
     width = 10, height = 10/1.5, units = "cm",
     res = 300, pointsize = 8)

dev.off()

#### Exportado de gráficos en pdf ####
pdf ("Graficos/Supuesto2.pdf",
     width = 10, height = 10/1.5,
     pointsize = 8)

dev.off()
