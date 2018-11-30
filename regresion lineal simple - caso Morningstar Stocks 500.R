l###############################
### Regresion lineal simple ###
###############################

#-------------------------------------------------------------------------------
# DESCRIPCION
#
# - FV : Morningstar's Fair Value
#        Estimación del valor por acción de una compañia que tiene en cuenta 
#        estimaciones de crecimiento, rentabilidad, riesgo y otros factores en 
#        un plazo de cinco años (Morningstar Stocks 500, edición de 2008).
#
# - SP : Share Price
#
# Morningstar, Inc. is an investment research and investment management firm 
# headquartered in Chicago, Illinois, United States.
#
# OBJETIVO
# Desarrollar una ecuación de regresión que pueda usarse para explicar el Share 
# Price en terminos del Fair Value
#
# Share Price = variable dependiente o respuesta (y)
# Fair Value  = variable independiente o explicativa (x)
#-------------------------------------------------------------------------------

# importar datos
Stocks500 <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/Stocks500.CSV", row.names=1)

# visualizar datos
View(Stocks500)

colnames(Stocks500) <- c("FV", "SP")

attach(Stocks500)

#------------------------------------#
# analisis expliratorio de los datos #
#------------------------------------#

# medidas de localizacion
summary(Stocks500) 
#       FV              SP        
# Min.   :15.00   Min.   : 11.02  
# 1st Qu.:37.25   1st Qu.: 32.19  
# Median :52.50   Median : 39.86  
# Mean   :54.46   Mean   : 46.64  
# 3rd Qu.:76.25   3rd Qu.: 62.55  
# Max.   :98.00   Max.   :103.05

# medidas de dispersion (coeficiente de variacion)
round( 100 * sd(FV) / mean(FV), 2 )
# 43.36%

round( 100 * sd(SP) / mean(SP), 2 )
# 52.6%

# las dos variables presentan una alta dispersion respecto al valor promedio

# coeficiente de correlacion
# asume valores entre -1 y 1
# ver Sosa et al. p. 120-121
round( 100 * cor(FV, SP), 2 )
# 87.72%
# la relacion (lineañ) entre la estimacion del valor de la accion (FV) y el valor
# real es directa y fuerte.

# histogramas
windows(width = 10, height = 5)
par( mfrow = c(1, 2) )
# variable dependiente
hist(x = SP, freq =  FALSE, col = "gray98", border = "darkgray", ylim = c(0, 0.023),
     xlab = "Share Price", ylab = "Densidad", main = "Share Price")
#lines(density(SP), col = "blue", lwd = 2)
# varaible independiente
hist(x = FV, freq =  FALSE, col = "gray98", border = "darkgray", ylim = c(0, 0.023), 
     xlab = "Fair Value", ylab = "Densidad", main = "Fair Value")
#lines(density(SP), col = "red", lwd = 2)


# dispersograma
windows()
plot(x = FV, y = SP, type = "p", pch = 16, col = "darkgreen", cex = 1.5,
      xlab = "Fair Price", ylab = "Share Price", main = "Share Price vs. Fair Price")
grid()

#-------------------------------------------------------------------------------
# Regresion lineal simple:
# - y    : vector de respuestas (cantidas aleatorias observadas).
# - X    : matriz de diseño (cantidades fijas obaservadas).
# - beta : coeficientes de la regresion (parametros, cantidades fijas desconocidas).
# - e    : vector de errores (cantidades aleatorias desconocidas)
# - Solo hay una variable de cada tipo.
# - Encontrar la mejor recta que caracterice (se ajuste) a la nube puntos, es 
#   decir, estimar el valor de beta_1 y beta_2 que minimicen los componentes del
#   error (aleatorio).
# - Se quiere estimar los coeficientes de la "mejor" linea recta y sigma^2.
#-------------------------------------------------------------------------------

#-------------------#
# ajuste del modelo #
#-------------------#

# modelo 1
m1 <- lm( formula = SP ~ FV )

# resumen
summary(m1)
# y^ = -2.99 + 0.91*x donde y = SP y x = FV
# Coeficiente de determinacion R^2 = 76.9%.
# Este modelo de regresion lineal representa bien la información.
# sigma^ = 12.01 

# percentil distribucion F
qf(p = 0.95, df1 = 1, df2 = 26, lower.tail = TRUE)


# suma de cuadrados total (SCT)
SCT <- sum( ( SP - mean(SP) )^2 )
# 16255.05

# suma de cuadrados de la regresion
SCR <- sum( ( m1$fitted.values - mean(SP) )^2  )
# 12507.03

SCE <- sum( ( SP - m1$fitted.values )^2 )
# 3748.02
# sum( ( m1$residuals )^2 )
# 3748.02
SCT - SCR
# 3748.02

# dispersograma con modelo 1
windows()
plot(x = FV, y = SP, type = "p", pch = 16, col = "darkgreen", cex = 1.5,
     xlab = "Fair Price", ylab = "Share Price", main = "Share Price vs. Fair Price")
grid()
abline(m1, col = "red")
lines(x = FV, y = m1$fitted.values, type = "p", col = "red", cex =  1.2, pch = 15)
legend("topleft", legend = c("Valores observados", "Valores ajustados"),
       col = c("darkgreen","red"), pch = c(16, 15))

# betas estimados del modelo 1
as.numeric( m1$coefficients )

#-------------------------------------------------------------------------------
# Interpretacion de los coeficientes:
#
# beta1^ = -2.99 (intercepto)
# De acuerdo con el modelo, el valor real de una compañia (Share Price) es 
# aproximadamente US -3.00 cuando el valor estimado por Morningstar (Fair Value)
# es de US 0.
#
# beta2^ = 0.91 (pendiente, tasa de cambio)
# De acuerdo con el modelo, el valor real de la compañia (Share Price) aumenta en
# US 0.91 por cada dolar de aumento de acuerdo a la estimacion
# del valor segun Morningstar (Fair Value) .
#-------------------------------------------------------------------------------

# residuales del modelo 1
m1$residuals

# valores ajustados del modelo 1
m1$fitted.values

# prediccion de acuerdo con el modelo 1
-2.99 + 0.91 * 40
# 33.41
# de acuerdo con el modelo, el valor real de una compañia avaluada en US 40 por 
# accion segun Morningstar es US 33.41

# grados de libertad residual
n - p
# m1$df.residual

# grados de libertad total
n - 1

# grados de libertad de la regresion
# p es el numero de coeficientes de la regresion (betas)
p - 1

# significancia de la pendiente (interpretacion)
# como la pendiente tiene un valor significativo, entones se concluye que el 
# impacto (efecto) de la estimación del precio de las acciones por Morningstar 
# (FV) es significativo sobre el precio real de las acciones (SP), es decir, la
# estimacion del precio de las acciones dada por Morningstar sirve efectivamente
# para caracterizar o explicar el precio real de las acciones de las compañias.

# modelo 2 (quitar el inercepto)
m2 <- lm(SP ~ -1 + FV)

# resumen
summary(m2)

# comparacion entre la desviacion estandar residual y la observada
s.SP <- sd( SP )
# 24.53648

# reduccion de la desviacion estandar incluyendo el modelo en ptos porcentuales
round( 100 * ( s.SP - 11.84 ) / s.SP, 2 )

# la reduccion de la desviacion estandar de la variable dependiente (SP) al 
# incluir la variable independiente (FV) es del 51%, lo que quiere decir que
# el modelo de regresión lineal efectivamente se ajusta a la información (el FV
# caracteriza significativamente el SP)

# dispersograma con modelos estimados
windows()
plot(x = FV, y = SP, type = "p", pch = 16, col = "darkgreen", cex = 1.5,
     xlab = "Fair Price", ylab = "Share Price", main = "Share Price vs. Fair Price")
grid()
abline(m1, col = "red") 
abline(m2, col = "blue") 
lines(x = FV, y = m2$fitted.values, type = "p", col = "blue", cex =  1.2, pch = 15)
legend("topleft", legend = c("Modelo 1", "Modelo 2"), col = c("red", "blue"), 
       lty = 1, lwd = 2, bg = "white")


#--------------#
# coeficientes #
#--------------#

# modelo 1
betas.m1 <- as.numeric( m1$coefficients )

b1.m1 <- betas.m1[1]
b2.m1 <- betas.m1[2]

# modelo 2
betas.m2 <- as.numeric( m2$coefficients )
# 0.8648457

b2.m2 <- betas.m2[1]
# 0.8648457

X <- as.matrix( FV )
y <- as.matrix( SP )
solve( t(X) %*% X ) %*% t(X) %*% y
# 0.8648457

#------------#
# prediccion #
#------------#

# prediccion del valor real de una compañia avaluada en US 50 por Morningstar
round( b1.m1 + b2.m1 * 50, 2 )
# US 42.58 por accion de acuerdo con el modelo 1

# prediccion del valor real de una compañia avaluada en US 50 por Morningstar
round( b2.m2 * 50, 2 )
# 43.24 por accion de acuerdo con el modelo 2

# valores ajustados modelo 2
y.hat <- m2$fitted.values

# intervalos de confianza dentro del rango del estudio
newx <- c(20, 40, 60, 80)
CIS  <- predict.lm(object = m2, newdata = data.frame( FV = newx ), se.fit = TRUE,
                   interval = "confidence", level = 0.95)

CIS$fit

# intervalos de confianza dentro del rango del estudio
newx <- seq(min(FV), max(FV), by = 0.05)
CIS  <- predict.lm(object = m2, newdata = data.frame( FV = newx ), se.fit = TRUE,
                   interval = "confidence", level = 0.95)

# desviacion estandar residual
CIS$residual.scale
# 11.84214

head( CIS$fit )

head( CIS$se.fit )

x0 <- as.matrix( min(FV) )
X  <- as.matrix( FV )
sig.hat <- CIS$residual.scale
sig.hat * sqrt( t(x0) %*% solve( t(X) %*% X ) %*% x0 )
# 0.5670806

# grafico
windows()
plot(x = FV, y = SP, type = "p", pch = 16, col = "darkgreen", cex = 1,
     xlab = "Fair Price", ylab = "Share Price", main = "Share Price vs. Fair Price")
grid()
abline(m2, col = "blue") 
lines(newx, CIS$fit[,2], col = "red", lty = 2)
lines(newx, CIS$fit[,3], col = "red", lty = 2)
legend("topleft", legend = c("Modelo ajustado", "Intervalos de confianza"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 1, bg = "white")


# intervalos de confianza fuera del rango del estudio
newx2 <- seq(max(FV), max(FV) + 25, by = 0.05)
CIS2  <- predict.lm(object = m2, newdata = data.frame( FV = newx2 ), se.fit = TRUE,
                   interval = "prediction", level = 0.95)

head( CIS2$fit )

head( CIS2$se.fit )

#-------------------------#
# diagnosticos del modelo #
#-------------------------#

# residuales delmodelo 2
res    <- m2$residuals

# residuales estandarizados del modelo 2
stdres <- rstandard(m2)

X <- as.matrix( FV )
H <- X %*%  solve( t(X) %*% X ) %*% t(X)

dim( X )

dim( H )

diag( H )

# promedio de los residuales estandarizados
mean( stdres )


# grafico residuales estandarizados vs variable independiente
# validar varianza constante y linealidad del modelo
windows()
plot(x = FV, y = stdres, ylim = c(-3, 3), col = "blue", xlab = "Fair Price",
     ylab = "Residual estandar", main = "Diagnostico de residuales")
grid()
abline(h = 0, col = "red", lty = 2)


# grafico residuales estandarizados vs valores ajustados
# validar varianza constante y linealidad del modelo
windows()
plot(x = y.hat, y = stdres, ylim = c(-3, 3), col = "blue", xlab = "Valor ajustado",
     ylab = "Residual estandar", main = "Diagnostico de residuales")
grid()
abline(h = 0, col = "red", lty = 2)


# grafico de residuales
# validad normalidad
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# histograma
curve(expr = dnorm(x, mean = 0, sd = 1), from = -3, to = 3, 
      col = "red", xlab = "Residual estandar", ylab = "Densidad", main = "Histograma")
hist(stdres, freq = FALSE, add = TRUE)
# qqplot
qqnorm(stdres)
qqline(stdres, col = "red")


# simulacion
grafico.original <- qqnorm(stdres)

windows()
plot(x = grafico.original$x, y = grafico.original$y, xlab = "Cuantiles Teoricos",
     ylab = "Cuantiles de la muestra")
for (i in 1:10000) {
     muestra.simulacion <- rnorm(n = 28, mean = 0, sd = 1)
     grafico <- qqnorm(muestra.simulacion, plot.it = FALSE)
     lines(x = grafico$x, y = grafico$y, type = "p", col = "lightgray", cex = 0.5)
}
lines(x = grafico.original$x, y = grafico.original$y, col = "blue", type = "p")
qqline(stdres, col = "red")


# prueba de normalidad
shapiro.test(stdres)


# prueba de Durbin Watson
# H0: los residuales son independientes
# frente a 
# H1: los residuales no son independientes
# install.packages("car")
library(car)
durbinWatsonTest(m2)

#----------#
# outliers #
#----------#

# observaciones con un residual alto

res    <- m2$residuals
stdres <- rstandard( m2 )
stures <- rstudent ( m2 )

Stocks500[stdres > 2, ]

Stocks500[stures > 3, ]

#---------------------------#
# observaciones influyentes #
#---------------------------#

# observaciones que pueden cambiar significativamente el ajuste del modelo

# modelo 2
n <- 28
p <- 1

DFFITS  <- dffits( m2 )
DFBETAS <- dfbeta( m2 )
COOKD   <- cooks.distance( m2 )
h       <- hatvalues(m2)  #diag(H)

Stocks500[DFFITS > 2 * sqrt(p/(n-p)), ]

Stocks500[DFBETAS > 2/sqrt(n), ]

Stocks500[COOKD > 1, ]

Stocks500[h > 3*p/n, ]

# grafico
windows()
plot(m2, which = 5)