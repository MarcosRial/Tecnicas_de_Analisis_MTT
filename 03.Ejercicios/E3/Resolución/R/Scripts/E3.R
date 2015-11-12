## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de Análisis Cuantitativas y Cualitativas
## Script de R para la realización del efercicio 3 de evaluación
## 04/11/2015
## Marcos Rial Docampo

#### Inicio del ejercicio ####
# Cargamos los datos
datos <- read.csv("Datos/DatosExerc2.csv")

# Eliminado de la variable categórica X
datos2 <- datos[,-1]

#### Relación entre las 3 variables ####
plot(datos2, main = "Relación entre variables",
     pch = 21, bg = "grey",
     cex = 0.8, cex.axis = 0.8)

# Otro método
triple <- par(mfrow = c(3,1))
par(triple)
plot(abandon.uaa~pop.dens, data = datos2,
     col = "darkgoldenrod3", bg = "azure4", pch = 21,
     xlab = "Densidad de pob. (hab./km2)",
     ylab = "Abandono de sup. agrícola")
plot(abandon.uaa~elevation, data = datos2,
     col = "darkgoldenrod3", bg = "sienna", pch = 21,
     xlab = "Elevación (msnm)",
     ylab = "Abandono de sup. agrícola")
plot(pop.dens~elevation, data = datos2,
     col = "darkolivegreen1", bg="burlywood3", pch = 21,
     xlab = "Elevación (msnm)",
     ylab = "Densidad de pob. (hab./km2)")

#### Estudio de correlación entre variables ####
# H0 = las variables no están correlacionadas
# Abandono vs densidad de población
cor.test(datos2$abandon.uaa, datos2$pop.dens,
         method = "pearson", conf.level = 0.95)
# Abandono vs elevación
cor.test(datos2$abandon.uaa, datos2$elevation,
         method = "pearson", conf.level = 0.95)
# Elevacion vs densidad de población
cor.test(datos2$elevation, datos2$pop.dens,
         method = "pearson", conf.level = 0.95)

#### Escalado de datos ####
datos3 <- scale(datos2)
summary(datos3)

#### Método de agrupamiento jerárquico ####
# Cálculo de distancias euclídeas
distancias <- dist(datos3, method = "euclidean")

# Agrupamientos y generación del dendrograma
agrup <- hclust(distancias, method = "ward.D")
plot(agrup, hang = -1, cex = 0.5,
     main = "Agrupamientos",
     ylab = "Peso",
     xlab = "", sub = "")
abline(h = 5, col = "red", lty = 2, lwd = 1)

# Generación de grupos (k=6)
grupos1 <- cutree(agrup, k = 5)

# Análisis de los grupos con boxplot
boxplot(abandon.uaa ~ grupos1, data = datos2,
        ylab = "Abandono de superficie agrícola",
        xlab = "Grupo")

boxplot(elevation ~ grupos1, data = datos2,
        ylab = "Elevación (msnm)",
        xlab = "Grupo")

boxplot(pop.dens ~ grupos1, data = datos2,
        ylab = "Densidad de población (hab./km2)",
        xlab = "Grupo")

#### Método de agrupamiento no jerárquico k-means####

#### Exportado de gráficos ####
png("Graficos/Dendrograma.png", #cambiar según convenga
    width=10, height=10/1.5, units="cm",
    res=300, pointsize=8)

dev.off()
