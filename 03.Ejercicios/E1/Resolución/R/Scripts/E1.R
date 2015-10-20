## Máster en Gestión Sostenible de la Tierra y el Territorio
## Técnicas de análisis cuantitativas y cualitativas
## Script de R para la realización del ejercicio 1 de evaluación
## 19/10/2015
## Marcos Rial Docampo

#n=502, error de muestreo +/-4.5%
#Aplicación de prop.test para definir los intervalos de confianza
#PP con 33.7% de los votos
prop.test(round(0.337*502, digits = 0),502,
          conf.level = 0.95)

#Cs con 13.8% de los votos
prop.test(round(0.138*502, digits = 0),502,
          conf.level = 0.95)

#AM con 29.1% de los votos
prop.test(round(0.291*502, digits = 0),502,
          conf.level = 0.95)

#PSOE con 17.4% de los votos
prop.test(round(0.174*502, digits = 0),502,
          conf.level = 0.95)
