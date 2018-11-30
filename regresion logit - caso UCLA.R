#######################################################
# EJEMPLO 1: Regresion logistica, tomado de UCLA,idre #
# https://stats.idre.ucla.edu/r/dae/logit-regression/ #
#######################################################

#-------------------------------------------------------------------------------
# Descripcion de la data:
# How variables, such as:
# - GRE  : Graduate Record Exam scores, 
# - GPA  : Grade point average, 
# - rank : Prestige of the undergraduate institution, 
# affect admission into graduate school. 
# The response variable, admit/don't admit, is a binary variable.
#-------------------------------------------------------------------------------

#####################
### Importar data ###
#####################

# establecer el directorio de trabajo
setwd("C:/Users/Toshiba/Dropbox/UE/Estadistica 2  II-2018/data/")

# importar la base de datos
adm <- read.table(file = "admission.csv", header = TRUE, sep = ",")

# visualizar la data
View(adm)

################################
#### Estadistica descriptiva ###
################################

# algunas medidas descriptivas
summary(adm)

# coeficientes de variacion
round( x = sapply(X = adm, FUN = sd)/colMeans(x = adm), digits = 3 )

# tabla de rank vs admit (frecuencias absolutas)
tab <- xtabs(formula = ~ admit + rank, data = adm)

# tabla de frecuencias relativas
print( tab.rel <- round( prop.table(x = tab, margin = 1), 3) )

print( tab ) 

# grafico tabla
windows()
barplot(tab.rel, beside = T, horiz = F, col = c("red","blue"),
        xlab = "Ranking", ylab = "Porcentaje", main = "Admisión por ranking",
        legend.text = c("No admitido","Admitido"))

# grafico dispersogramas
colors <- c("red","blue")[adm$admit + 1]  # + 1 para asignar los colores

windows()
pairs(adm[ , -1], pch = 1, cex = 0.8, gap = 0, col = colors, 
      xaxt = "n", yaxt = "n", labels = c("GRE","GPA","Ranking"))

# boxplots
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
boxplot(gre ~ admit, data = adm, col = c("red","blue"),
        main = "GRE por admsion", xlab = "Admision")
boxplot(gpa ~ admit, data = adm, col = c("red","blue"),
        main = "GPA por admsion", xlab = "Admision")

########################
### Modelo logistico ###
########################

# convertir rank en un factor para ser tratada como una categorica
adm$rank <- factor(adm$rank)

# ajuste del modlo
mylogit <- glm(admit ~ gre + gpa + rank, data = adm, family = "binomial")

# ver los resultados
summary(mylogit)

#-------------------------------------------------------------------------------
#   INTERPRETACION
# - Todas las variables resultan ser significativas.
# - Los parametros representan el cambio en el log odds de ser admitidos para un 
#   aumento de una unidad de la correspondiente variable indepependiente
#   (mientras el resto de las variables se mantienen constantes).
#   Por cada unidad de cambio en el GRE, el log odds de ser admitido (frente a 
#   no ser admitido) en un posgrado aumentan en 0.002.
#   Por cada unidad de cambio en el GPA, el log odds de ser admitido en un 
#   posgrado aumentan en 0.804.
#   Haber asistido a una institución de pregrado con ranking 2, frente a una 
#   institución con ranking 1, cambia el log odds de ser admitido en -0.675.
#-------------------------------------------------------------------------------

# intervalos de confianza para los coeficientes
round(x = confint(mylogit), digits = 5)

# intervalos de confianza para los Odds Ratios (ORs)
round(x = exp( cbind( coef(mylogit), confint(mylogit) ) ), digits = 5)

#-------------------------------------------------------------------------------
#   INTERPRETACION
# - Por ej., ahora decimos que para un aumento de una unidad en GPA, el chance 
#   (odds) de ser admitido en un posgrado (frente a no ser admitido) aumentan en 
#   un factor de 2.23.
#-------------------------------------------------------------------------------

####################################
#### Bondad de ajuste del modelo ###
####################################

with(data = mylogit, 
     expr = pchisq(q = null.deviance - deviance, df = df.null - df.residual, 
                   lower.tail = F))

#-------------------------------------------------------------------------------
#   INTERPRETACION
# - Un valor de p de menos de 0.001 indica que el modelo en conjunto ajusta 
#   significativamente mejor que un modelo vacío (sin predictores).
#   Esto se denomina prueba de razón de verosimilitud.
#-------------------------------------------------------------------------------

##################
### Prediccion ###
##################

# data para prediccion/clasificacion
D <- with(data = adm, 
          expr = data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

# visualizar la data para prediccion/clasificacion
print( D )

# agregar en la data una una columna con las probabilidades predichas
D$prediction <- predict(mylogit, newdata = D, type = "response")

# visualizar predictores y probabilidades predichas
print( D )

#-------------------------------------------------------------------------------
#   INTERPRETACION
# - La probabilidad predicha de ser aceptado en un programa de postgrado es de 
#   0.52 para estudiantes de instituciones de pregrado de mayor prestigio y 0.18 
#   para estudiantes de las instituciones de menor rango, manteniendo GRE y GPA 
#   en sus valores promedio.
#-------------------------------------------------------------------------------

###########################
### matriz de confusion ###
###########################

# predecir las probabilidades de admision usando la data original
prob.pred <- predict(mylogit, newdata = adm[ , -1], type = "response")

# clasificacion: punto de corte = 0.5
y.pred <- rep(0, length(prob.pred))
y.pred[prob.pred > 0.5] <- 1

# clasificacion: punto de corte = 0.3
y.pred <- rep(0, length(prob.pred))
y.pred[prob.pred > 0.3] <- 1

# matriz de confusion (frecuencias absolutas)
.. <- table(adm$admit, y.pred, dnn = c("Admision","Prediccion"))

n  <- length(prob.pred)  # tamaño de la muestra
TN <- tab[1,1] / n       # True  Negative rate
TP <- tab[2,2] / n       # True  Positive rate
FP <- tab[1,2] / n       # False Positive rate
FN <- tab[2,1] / n       # False Negative rate

#-------------------------------------------------------------------------------
# .Ver medidas de descripcion de matrices de confusion y curvas ROC (Receiver 
# Operating Characteristic). 
# - https://en.wikipedia.org/wiki/Confusion_matrix
# - https://en.wikipedia.org/wiki/Receiver_operating_characteristic
#-------------------------------------------------------------------------------