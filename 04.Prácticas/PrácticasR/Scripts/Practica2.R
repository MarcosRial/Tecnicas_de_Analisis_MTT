## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización de la práctica 2. Contrastes de Hipótesis
## 08/10/2015
## Marcos Rial Docampo

#Importación de datos
datos<-read.csv("Datos/DatosClase.csv")


#Contraste de hipótesis para
#H0: media(altura)<=170cm
#H1: media(altura)>170cm
t.test(datos$altura, mu=170,
       alternative = "greater")

#Contraste de hipótesis para
#H0: media(h-hombres)=media(h-mujeres)
#H1: media(h-hombres)!=media(h-mujeres)
t.test(datos$altura~datos$sexo,
       alternative = "two.sided")

#Muestra n=50, 35 no defectuosos
#H1: prop no defectuosos (p) < 80%
prop.test(35, 50, p=0.8,
          alternative="less")

#Muestra n=50+50=100, 35+37=72 no defectuosos
#H1: prop no defectuosos (p) < 80%
prop.test(72, 100, p=0.8,
          alternative="less")