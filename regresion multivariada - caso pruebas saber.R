#############
# Ejemplo 2 #
#############

#install.packages("heaavy")
library(heavy)

#establecer el directorio de trabajo
setwd("C:/Users/Toshiba/Dropbox/UE/Estadistica 2  II-2018/data/")

#importar datos
saber15 <- read.csv(file = "saber11_2015_1.csv", header = TRUE, sep = ",")

#matriz de datos
X <- saber15[ , c("PROMLECTURACRITICA","PROMSOCIALESYCIUDADANAS",
                  "PROMCOMPETENCIASCIUDADAN","PROMMATEMATICA",
                  "PROMRAZONAMIENTOCUANTITA","PROMCIENCIASNATURALES")]

X <- X[-c(222), ]

#ajustar modelos lineales independientemente
fit.lec <- heavyLm(PROMLECTURACRITICA ~ PROMMATEMATICA + PROMRAZONAMIENTOCUANTITA + PROMCIENCIASNATURALES, data = X)

fit.soc <- heavyLm(PROMSOCIALESYCIUDADANAS ~ PROMMATEMATICA + PROMRAZONAMIENTOCUANTITA + PROMCIENCIASNATURALES, data = X)

fit.com <- heavyLm(PROMCOMPETENCIASCIUDADAN ~ PROMRAZONAMIENTOCUANTITA + PROMCIENCIASNATURALES, data = X)

#matriz de residuales
residuales <- data.frame(fit.lec$residuals, fit.soc$residuals, fit.com$residuals)
colnames(residuales) <- c("relec","resoc","recom")

#prisma de correlaciones
windows()
pairs(residuales)

#El objeto es ver si queda información adicional entre las variables dependientes 
#después de tener en cuenta los efectos lineales de las variables explicativas

pca <- prcomp(residuales, scale = TRUE)

print(pca, digits = 3)

summary(pca)

#INTERPRETACION
#las primeras dos componentes captan casi la totalidad de la variabilidad
#la primera componente es la mas relevante
#las cargas/pesos se pueden interpretar aqui como coeficientes de correlacion 
#entre los residuales
#en la primera componente los tres residuales tienen una correlacion moderada
#la segunda componente esta compuesta casi enteramente por residuales de lectura

windows()
biplot(prcomp(residuales, scale = TRUE), cex = c(.6, 1.2), 
       col  = c(3, 4), cex.lab=1.5,
       xlab = "First principal component",
       ylab = "Second principal component")
