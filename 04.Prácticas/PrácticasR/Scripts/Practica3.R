## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización de la práctica 3
## 08/10/2015
## Marcos Rial Docampo

#Cargado de librería MASS para datos de ejemplo
library(MASS)

#Cargado de datos de ejemplo
data(caith)

#Evaluación de clase y contenidos
class(caith)
str(caith)

#Cambio de nombres de columna
colnames(caith) <- c("rubio","pelirrojo", "castaño", "moreno", "negro")
#...y fila
rownames(caith) <- c("azules", "claros", "castaños", "oscuros")

#Convertir caith a tipo table
caith.table <- as.table(as.matrix(caith))
#Comprobación de tipo de objeto
class(caith.table)

#Calcular frecuencias relativas
prop.table(caith.table) #globales (referidas al total)
round(prop.table(caith.table, 1),2) #referidas a filas
100*round(prop.table(caith.table, 2),2) #referidas a columnas

#Contraste de independencia de caracteres
chisq.test(caith.table)

#Gráfico
plot(caith.table, col=2:6)

#Hacemos un análisis de correspondencias
plot(corresp(caith, nf=2))
