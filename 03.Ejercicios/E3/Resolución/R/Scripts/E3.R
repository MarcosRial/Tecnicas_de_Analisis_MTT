## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de Análisis Cuantitativas y Cualitativas
## Script de R para la realización del efercicio 3 de evaluación
## 04/11/2015
## Marcos Rial Docampo

#### Inicio del ejercicio ####
# Cargamos los datos
datos <- read.csv("Datos/DatosExerc2.csv")

# Escalado de datos
datos2 <- scale(datos)
summary(datos2)

#### Método de agrupamiento jerárquico ####
# Cálculo de distancias euclídeas
distancias <- dist(datos2)

# Agrupamientos y generación del dendrograma
agrup <- hclust(distancias, method = "ward")
plot(agrup, hang = -1, cex = 0.5,
     main = "Agrupamientos",
     xlab = "", sub = "")

# Generación de grupos (k=6)
grupos1 <- cutree(agrup, k = 6)

#### Método de agrupamiento no jerárquico ####

#### Exportado de gráficos ####
png("Graficos/Dendrograma.png", #cambiar según convenga
    width=10, height=10/1.5, units="cm",
    res=300, pointsize=8)

dev.off()
