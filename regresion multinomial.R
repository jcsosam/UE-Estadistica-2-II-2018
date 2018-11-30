###############################################################
# EJEMPLO 2: Regresion multinomial, Zelterman (2015), Cap. 10 #
###############################################################

#-------------------------------------------------------------------------------
# DESCRIPCION DE LA DATA
# Datos sobre tres variedades de cultivo de vino (Forina et al, 1988). Hay 178 
# vinos diferentes examinados y 13 concentraciones químicas que describen 
# las muestras de vino.
# La base de datos contiene las siguientes vairables:
# - Class    Which of three cultivars of wine grapes
# - Alcohol  Alcohol content
# - Malic    Malic acid: provides a sour taste
# - Ash      Ash content
# - Alcal    Alcalinity of ash
# - Mg       Magnesium content
# - Phenol   Total phenols: compounds found in drugs and plastics
# - Flav     Flavanoids: compounds found widely in plants
# - Nonf     Nonflavanoid phenols
# - Proan    Proanthocyanins: tannins which affect color and aging
# - Color
# - Hue
# - Abs      Ratio of light absorption at two different frequencies
# - Proline  Proline, an amino acid
#-------------------------------------------------------------------------------

#################
### Librerias ###
#################

library(MASS)
library(nnet)  # regresion multinomial

#####################
### Importar data ###
#####################

# establecer el directorio de trabajo
setwd("C:/Users/Toshiba/Dropbox/UE/Estadistica 2  II-2018/data/")

# importar la base de datos
wines <- read.table("wines.txt", header = TRUE, sep = "\t")

# visualizar la data
View(wines)

n <- nrow(wines)  # numero de individuos
p <- ncol(wines)  # numero de variables

###############################
### Estadistica descriptiva ###
###############################

colors  <- c("green", "red", "blue")[wines$Class]

# prisma
windows()
pairs(wines, pch = 16, cex = .3, gap = 0, col = colors, xaxt = "n", yaxt = "n")

# perfiles
windows()
parcoord(wines[, c(9, 11, 12, 13, 14, 7, 8)], col = colors)

#-------------------------------------------------------------------------------
# INTERPRETACION
# Hay variables individuales que demuestran como difieren en los tres grupos de 
# vinos. El uso del color ayuda a identificar las variables que tienen un alto 
# valor en la discriminación entre las tres clases.
# Es posible cuantificar estas relaciones individuales en términos de un 
# análisis de varainza univariado por separado (esto no identificaria 
# características discriminatorias para los individuos).
#-------------------------------------------------------------------------------

# analisis de variaza univariado
for (i in 2:p) {
     z    <- summary.aov( aov(wines[ , i] ~ wines$Class) )   # ANOVA
     pval <- max( z[[1]]$"Pr(>F)"[1], 0.0001 )               # valor p
     m    <- rep(0, 3)                                       # medias
     for(j in 1:3) {
          m[j] <- mean( wines[ wines$Class == j, i] )
     }
     # configurar tabla
     if (i == 2) { univ <- c(pval, m) } else { univ <- rbind(univ, c(pval, m)) }
     # remover objetos
     rm(z, pval, m)
}
row.names(univ) <- colnames(wines)[2:p]
colnames(univ)  <- c("p-value","Group 1","Group 2","Groupo 3")

# visualizar tabla (univ)
print(x = univ, digits = 2)

####################################
### Modelo multinomial logistico ###
####################################

#-------------------------------------------------------------------------------
# ENFOQUE
# Seleccionae una categoría como referencia y compara todas las demas categorias 
# contra esta.
#-------------------------------------------------------------------------------

# Seleccionando la categoria 2 como baseline

wines$Class  <- as.factor(wines$Class)         # tratar Class como categorica
wines$rClass <- relevel(wines$Class, ref = 2)  # establecer la cat de referencia

# juste del modelo
winelogit <- multinom(rClass ~ Alcohol + Ash + Alcal + Abs  + Proline, 
                      data = wines, maxit = 200)

# vizualizar el output del modelo
print(ws <- summary(winelogit), digits = 4)

#-------------------------------------------------------------------------------
# INTERPRETACION
# Los coeficientes representan el cambio en el log odds de las probabilidades de 
# clasificación cuando la variable independiente cambia en una unidad. Esta es 
# la misma interpretación dada en regresión logística.
#-------------------------------------------------------------------------------

# p valores para los coeficientes
tratio <- ws$coefficients / ws$standard.errors
print( 2 * ( pt(q = abs(tratio), df = ws$edf, lower.tail = F) ), digits = 4)

# valores ajustados (probabilidades)
head( ws$fitted.values )

#-------------------------------------------------------------------------------
# INTERPRETACION
# Cada fila suma 1 y corresponde a las probabilidades estimadas de membresía
# para un caso especifico.
#-------------------------------------------------------------------------------

####################################
#### Bondad de ajuste del modelo ###
####################################

# grafico probabilidades ajustadas
windows()
plot(winelogit$fitted.values[ , 2:3], col = colors, cex = 1.25, 
     xlab = "Prob. ajustada Clase 1", ylab = "Prob. ajustada Clase 3")
legend("topright", fill = c("green", "red", "blue"), 
       legend = c("Cat. 1","Cat. 2 (ref)","Cat. 3"))
lines(c(0, 1), c(1, 0), lty = "dotted")
lines(c(0, 0), c(1, 0), lty = "dotted")
lines(c(1, 0), c(0, 0), lty = "dotted")