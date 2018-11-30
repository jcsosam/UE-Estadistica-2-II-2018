#################################
# Caso: Forbes Global 2000 List #
#################################

#-------------------------------------------------------------------------------
# Descripcion de la base de datos:
# - Rank    : Ranking
# - Company : Nombre de la compañia
# - Sales	  : Ventas
# - Profits : Ganancias
# - Assets  : Activos
# - Value   : Valor de la compañia en el mercado (abril 7, 2017)
# Nota: todas las cifras estan dadas en Billones (miles de millones) de dolares
# Fuente: https://www.forbes.com/global2000/list
#
# Mas info: "2017 Global 2000 Methodology: How We Crunch the Numbers"
#
# Objetivo: 
# - Describir numerica y graficamente las variables y sus relaciones.
# - Detectar patrones y anomalias.
# - Reducir (si es posible) la dimension de los datos.
#-------------------------------------------------------------------------------

#################
# base de datos #
#################

# importar la base de datos

View(FG)

# encabezado de la base de datos
head(FG)

dim(FG)
# 2000 registros
# 7 variables  

# variables
names(FG)

colnames(FG) <- c("Rank","Company","Country","Sales","Profits","Assets","Value")

n <- nrow(FG)  # numero de registros
p <- ncol(FG)  # numero de variables

# hay datos faltantes?
any(is.na(FG))
# TRUE
# Hay compañias que tienen datos faltantes!

FG[!complete.cases(FG), ]
# ver la fuente!

# pocentaje de datos faltantes
100 * sum( as.numeric( !complete.cases(FG) ) )/n
# El 0.2% de las compañias tiene datos faltantes

#-------------------------------------------------------------------------------

# remover datos faltantes
FG <- FG[complete.cases(FG), ]

# hay datos faltantes?
any(is.na(FG))
# FALSE
# Ya no hay datos faltantes!

dim(FG)
# 1996 registros
# 7    variables

n <- nrow(FG)  # numero de registros

# adjuntar base de datos
# trabajar directamente con las variables de FG
attach(FG)

#-------------------------------------------------------------------------------

#####################
# algunas consultas #
#####################

# compañias mas "valiosas"
# datos de las compañias con activos y valor superiores al percentil 99
p99.value <- quantile(Value, probs = 0.99)

p99.assets <- quantile(Assets, probs = 0.99)

FG[(Assets > p99.assets) & (Value > p99.value), ]

#-------------------------------------------------------------------------------

# datos de las compañias colombianas
FG[Country == "Colombia", ]

#-------------------------------------------------------------------------------

# datos de las compañias de sudamerica
paises.sura <- c("Argentina" , "Bolivia" , "Brazil"   , "Chile" , "Colombia", 
                 "Ecuador"   , "Guyana"  , "Paraguay" , "Peru"  , "Surinam" , 
                 "Uruguay"   , "Venezuela")

FG.sura <- FG[Country %in% paises.sura, ]

dim(FG.sura)
# 40  7
# hay 40 compañias en el listado

head(x = FG.sura, n = 10)

#-------------------------------------------------------------------------------

# ordenar (A-Z) por "Country"
FG.sura[order(FG.sura$Country), ]

# ordenar (descendentemente) por "Value"
FG.sura[order(FG.sura$Value, decreasing = TRUE), ]

unique(FG.sura$Country)

#-------------------------------------------------------------------------------

##########################
# variables cualitativas #
##########################

# tabla de frecuencias (absolutas)
tabla <- table(Country)

sum( tabla )

length(tabla)
# 61 paises en la lista Forbes Globe

#-------------------------------------------------------------------------------

# tabla ordenada descendentemente de acuerdo a la frecuencia
tabla <- tabla[order(tabla, decreasing = TRUE)]

length(tabla)

# top 5 de los paises con mayor cantidad de compañias en la lista
tabla[1:5]

round( 100 * tabla[1:5] / n, 2)

#-------------------------------------------------------------------------------

# posicion de colombia de acuerdo a la cantidad de compañias
which(names(tabla) == "Colombia")

which(names(tabla) == "Venezuela")

which(names(tabla) == "Brazil")

which(names(tabla) == "Mexico")

which(names(tabla) == "Peru")

# top 5
tabla[c(1, 2, 3, 4, 5)]

tail(tabla)

#-------------------------------------------------------------------------------

# total de paises
length(tabla)
# 61

# total de compañias
sum(tabla)
# 1996

identical(sum(tabla), n)
# TRUE
# la suma de las frecuencias de la tabla es identica a n

#-------------------------------------------------------------------------------

# top 5 de acuerdo a la cantidad de compañias
tabla.top5 <- tabla[1:5]

# top 5 con frecuencias relativas (proporciones)
tabla.top5.rel <- round(tabla.top5 / n, 3)

# mas de la mitad de las FG 2000 son solo de 5 paises
sum( tabla.top5.rel )
# el 57.5% de las compañias del ranking pertenecen unicamente a cinco paises

#-------------------------------------------------------------------------------

# grafico de barras
windows()
barplot(height = tabla.top5.rel)

#-------------------------------------------------------------------------------

windows()
barplot(height = 100 * tabla.top5.rel, names.arg = c("US", "JP", "CN", "UK", "SK"),
        col = "orange", border = "blue", xlab = "País", ylab = "Porcentaje (%)", 
        main = "Países Top 5 Forbes Globe 2000", font.main = 4, density = 25)

#-------------------------------------------------------------------------------

# mas informacion
?barplot

# pegar grafico en Microsoft Word

#-------------------------------------------------------------------------------

###########################
# variables cuantitativas #
###########################

# cargar 'resumen_univariado.R'
source(file = "C:/Users/Juan Camilo/Dropbox/UE/MCGIF/code/resumen_univariado.R")

resumen.profits <- mi.resumen.uni(x = Profits, r = 1)

#-------------------------------------------------------------------------------

# base de datos solo con variables cuantitativas
FG.cuanti <- FG[ , c("Sales", "Profits", "Assets", "Value")]

head(FG.cuanti)

#-------------------------------------------------------------------------------

# resumen multivariado
summary(FG.cuanti)

# coeficiente de variacion 
round( apply(X = FG.cuanti, MARGIN = 2, FUN = function(x) 100 * sd(x)/mean(x)), 1)

#-------------------------------------------------------------------------------

# graficos: examinar la distribucion de una variable
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# diagramas de caja
boxplot(x = Profits, horizontal = TRUE, boxwex = 0.5, cex = 1.1, 
        border = "blue", col = "lightblue")
# histograma
hist(x = Profits, freq = FALSE, nclass = 50, border = "blue", col = "lightblue", 
     xlab = " ", ylab = "Densidad", main = " ")
# titulo
title(main = "Ganancias Compañías Forbes Globe 2000", font.main = 4, outer = TRUE, line = -2)

#-------------------------------------------------------------------------------

# rango intercuartilico profits
q3.profits <- quantile(x = Profits, probs = 0.75)

q1.profits <- quantile(x = Profits, probs = 0.25)

RI.profits <- as.numeric( q3.profits - q1.profits )

#-------------------------------------------------------------------------------

# cantidad de datos atipicos superiores     
sum( as.numeric(Profits > q3.profits + 1.5 * RI.profits) )

round(100 * 208/n, 2)
# el 10.42% de las compañia presenta valores atípicos en las ganancias

#-------------------------------------------------------------------------------

# cantidad de datos extremos superiores
sum( as.numeric(Profits > q3.profits + 3.0 * RI.profits) )

round(100 * 125/n, 2)
# el 6.26% de las compañia presenta valores extremos en las ganancias

#-------------------------------------------------------------------------------

# datos de compañias extremas superiores 
FG.extrem.sup <- FG[Profits > q3.profits + 3.0 * RI.profits, c("Company", "Country", "Profits")]

head(x = FG.extrem.sup[order(FG.extrem.sup$Profits, decreasing = TRUE), ], n = 7)

#-------------------------------------------------------------------------------

# base de datos sudamerica solo con variables cuantitativas
FG.cuanti.sura <- FG[Country %in% paises.sura , c("Sales", "Profits", "Assets", "Value")]

head(FG.cuanti.sura)

dim(FG.cuanti.sura)

#-------------------------------------------------------------------------------

# diagramas de caja compañias suramerica
windows()
boxplot(FG.cuanti.sura, horizontal = TRUE, boxwex = 0.5, cex = 0.8, las = 1,
        border = c("darkgreen", "blue", "red", "purple4"),
        col = c("lavender", "lightblue", "mistyrose", "linen"), 
        xlab = "Billones de dolares", main = "Compañías Sudamérica Forbes Globe 2000")

#-------------------------------------------------------------------------------

# dispersograma o nube de puntos
# examinar la relacion entre variable cuantitativas
windows()
plot(Sales, Profits)


windows()
plot(x = Sales, y = Profits, cex = 1.2, pch = 16, col = "darkgreen", 
     ylab = "Ganancias", xlab = "Ventas", main = "Ganancias frente a Ventas")
grid()

#-------------------------------------------------------------------------------

# prisma de dispersogramas
windows()
pairs(FG.cuanti)

windows()
pairs(FG.cuanti, pch = 16, cex = 0.8, gap = 0, xaxt = "n", yaxt = "n", 
      col = "blue", labels = c("Ventas", "Ganancias", "Activos", "Valor")) 

#-------------------------------------------------------------------------------

# covarianza
# el signo de la covarianza indica el tipo relacion entre las variables
# positivo: directa
# negativo: inversa
# unidades mixtas
round(cov(Sales, Profits), 1)

# matriz de covarianza
round(cov(FG.cuanti), 1)

#-------------------------------------------------------------------------------

# coeficiente de correlacion de Pearson
# cuantifica el grado de relacion lineal entre dos variables cuantitativas
# siempre toma valores entre -1 y 1
# entre mas cercano a  1 la relacion es directa y fuerte
# entre mas cercano a -1 la relacion es inversa y fuerte
# entre mas cercano a  0 la relacion es mas debil
# es adimensional (no tiene unidades)
round(cor(Sales, Profits), 2)

# matriz de correlacion
round(cor(FG.cuanti), 2)

#-------------------------------------------------------------------------------

# categorizar variables cuantitativas

# categorizar Profits
max(Profits)

min(Profits)

Profits.categorias <- cut(x = Profits, breaks = seq(from = -13, to = 45.2, len = 5) )

head(Profits.categorias)

table( Profits.categorias )

# categorizar Sales
max(Sales)

min(Sales)

Sales.categorias <- cut(x = Sales, breaks = seq(from = 0.001, to = 485.3, len = 5) )

# tabla cruzada (de contingencia)

round( 100 * table( Profits.categorias, Sales.categorias) / n, 2 )

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------