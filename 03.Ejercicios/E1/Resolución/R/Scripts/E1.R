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

#Alternativo 1
##PP y Cs conjuntamente con un 47.5% de los votos
prop.test(round(0.475*502, digits = 0),502,
          conf.level = 0.95)

#Alternativo 2
#Contraste de hipótesis
#PP con 33.7% de los votos frente al 34.6% obtenido en las elecciones
prop.test(round(0.337*502, digits = 0),502,
          p = 0.346, alternative = "two.sided",
          conf.level = 0.95)

#Cs con 13.8% de los votos frente al 11.4% obtenido en las elecciones
prop.test(round(0.138*502, digits = 0),502,
          p = 0.114, alternative = "two.sided",
          conf.level = 0.95)

#AM con 29.1% de los votos frente al 31.9% obtenido en las elecciones
prop.test(round(0.291*502, digits = 0),502,
          p = 0.319, alternative = "two.sided",
          conf.level = 0.95)

#PSOE con 17.4% de los votos frente al 15.3% obtenido en las elecciones
prop.test(round(0.174*502, digits = 0),502,
          p = 0.153, alternative = "two.sided",
          conf.level = 0.95)