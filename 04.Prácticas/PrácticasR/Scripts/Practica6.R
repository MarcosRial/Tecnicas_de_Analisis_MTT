## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización de la práctica 6. Análisis de conglomerados.
## 22/10/2015
## Marcos Rial Docampo

### Preliminares
# Cargamos y exploramos los datos de ejemplo
data(mtcars)
str(mtcars)
head(mtcars)

# Para obtener información sobre los datos
help(mtcars)

# Exploramos la correlación entre las variables
cor(mtcars, method="spearman")

# Exploramos las posibles diferencias de escala
summary(mtcars)

# Estandarizamos las variables
mtcars2 <- scale(mtcars)
summary(mtcars2)

## Agrupamiento jerárquico
# Cálculo de la distancia entre observaciones
distancias <- dist(mtcars2)
#Y por el método manhattan
distancias.m <- dist(mtcars2, method = "manhattan")

# Podemos explorar las diferentes medidas de distancia disponibles
help(dist)

# Agrupamiento
agrupam    <- hclust(distancias, method="ward.D")
# Y sobre distancias.m
agrupam.m    <- hclust(distancias.m, method="ward.D")
# Diferentes métodos de agrupamiento disponibles
help(hclust)

# Representación del dengrograma
plot(agrupam, hang=-1, cex=0.8)
plot(agrupam.m, hang=-1, cex=0.8)

# Decidimos formar 4 grupos
grupos1 <- cutree(agrupam, k=4)
grupos1m <- cutree(agrupam.m, k=4)
print(grupos1)
print(grupos1m)

# Comparamos grupos1 y grupos1m
table(grupos1, grupos1m)

# Tratamos de interpretar los resultados, p.ej. con el consumo y el peso
tapply(mtcars$mpg, INDEX=grupos1, mean)
tapply(mtcars$wt, INDEX=grupos1, mean)

# También podemos intepretarlo usando diagramas de cajas
boxplot(mpg~grupos1, data=mtcars, ylab="Consumo (millas/galón)")
boxplot(wt~grupos1, data=mtcars, ylab="Peso (miles de libras)")
boxplot(hp~grupos1, data=mtcars, ylab="Potencia (cv)")
boxplot(qsec~grupos1, data=mtcars, ylab="Tiempo 1/4 milla (seg)")

## Generar un gráfico de alta calidad
png("Graficos/graficoEjem.png",
    width=10, height=10/1.5, units="cm",
    res=300, pointsize=10)
boxplot(mpg~grupos1, data=mtcars, ylab="Consumo (millas/galón)")
dev.off()

## O, en pdf:
pdf("Graficos/graficoEjem.pdf", width=10, height=10/1.5)
boxplot(hp~grupos1, data=mtcars, ylab="Potencia (cv)")
boxplot(mpg~grupos1, data=mtcars, ylab="Consumo (millas/galón)")
dev.off()


######################
## Inciso: generar gráficos por lotes
for(i in 1:11) {
  png(paste("Graficos/grafico",i,".png", sep=""), 
      width=10, height=10/1.5, units="cm", res=300, pointsize=10)
  boxplot(mtcars[,i]~grupos1,
          ylab=colnames(mtcars)[i])
  dev.off()
}
######################



## Agrupamiento no jerárquico: k-medias
grupos2 <- kmeans(mtcars2, centers=4)
print(grupos2$cluster)

# Comparación con el resultado del agrupamiento jerárquico
table(grupos1, grupos2$cluster)