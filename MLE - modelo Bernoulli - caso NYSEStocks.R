############################
### MAXIMA VEROSIMILITUD ###
############################

# (Anderson 2011, Chap. 8., problem 47) Many stock market observers say that 
# when the PE ratio for stocks gets over 20 the market is overvalued. The PE 
# ratio is the stock price divided by the most recent 12 months of earnings.
# Suppose you are interested in seeing whether the current market is overvalued 
# and would also like to know what proportion of companies pay dividends. 
# A random sample of 30 companies listed on the New York Stock Exchange (NYSE) 
# is provided (Barron's, January 19, 2004).

# Objetivo: hacer inferencia sobre la proporcion poblacional de companias que
# efectivamente si producen dividendos.

# importar datos
NYSEStocks <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/NYSEStocks.CSV")

# vizualizar datos
View(NYSEStocks)

# renombrar las variables
colnames(NYSEStocks) <- c("Compania","Dividendos","PE")

# hay datos faltantes?
any( is.na(NYSEStocks) )
# FALSE
# no hay companias con datos faltantes

n <- nrow(NYSEStocks)  # tamaño de la muestra
# Hay 30 companias en la muestra

# adjuntar la base de dastos
attach(NYSEStocks)

# varible de estudio
x <- rep(NA, n)
x[Dividendos == "Yes"] <- 1
x[Dividendos == "No"] <- 0


############################
# Calcular el MLE de theta #
############################

s <- sum(x)  # estadistico suficiente 
# Hay 21 companias que si producen divendos.

theta.hat <- s / n  # estumacion maximo verosimil de theta
# Se estima que la proporcion de companias que si producen dividendos es 70%.

#theta.hat <- mean(x)

######################################
# Informacion de Observada de Fisher #
######################################

bernoulli.loglikelihood.primeraderivada <- function( theta, n, s ) 
{
     
     return( s / theta - (n - s)/(1 - theta) )
     
}

bernoulli.loglikelihood.segundaderivada <- function( theta, n, s ) 
{
     
     return( -s / theta^2 - (n - s)/(1 - theta)^2 )
     
}

# grafico de la derivada y de la segunda derivada
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# primera
curve(expr = bernoulli.loglikelihood.primeraderivada(x, n = 30, s = 21), 
      from = 0.4, to = 0.9, col = "black", 
      xlab = expression(theta), ylab = "Derivada")
curve(expr = bernoulli.loglikelihood.primeraderivada(x, n = 60, s = 42),
      col = "green", add = TRUE)
grid()
abline(v = theta.hat, col = "blue", lwd = 2)
# segunda
curve(expr = bernoulli.loglikelihood.segundaderivada(x, n = 30, s = 21), 
      from = 0.4, to = 0.9, col = "black", 
      xlab = expression(theta), ylab = "Segunda derivada")
curve(expr = bernoulli.loglikelihood.segundaderivada(x, n = 60, s = 42), col = "green", add = TRUE)
grid()
abline(v = theta.hat, col = "blue", lwd = 2)


####################################################
# Graficar la verosimilitud y la log-verosimilitud #
####################################################

# funcion de log-verosimilitud
bernoulli.loglikelihood <- function( theta, n, s ) {
     
     return( s * log( theta ) + ( n - s ) * log( 1 - theta ) )
     
}

# funcion de verosimilitud
bernoulli.likelihood <- function( theta, n, s ) {
     
     return( exp( bernoulli.loglikelihood( theta, n, s ) ) )
     
}

# grafico de la verosimilitud
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# grafico 1
curve(expr = bernoulli.likelihood(x, n = 30, s = 21), 
      from = 0, to = 1, col = "black", 
      xlab = "theta", ylab = "Funcion de Verosimilitud")
grid()
abline(v = theta.hat, col = "blue", lwd = 2)
# grafico 2
curve(expr = bernoulli.likelihood(x, n = 30, s = 21), 
      from = 0.4, to = 0.9, col = "black", 
      xlab = "theta", ylab = "Funcion de Verosimilitud")
grid()
abline(v = theta.hat, col = "blue", lwd = 2)

# grafico de la logverosimilitud
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# grafico 1
curve(expr = bernoulli.loglikelihood(x, n = 30, s = 21), 
      from = 0.01, to = 0.99, col = "black", 
      xlab = "theta", ylab = "Funcion de Log-Verosimilitud")
grid()
abline(v = theta.hat, col = "blue", lwd = 2)
# grafico 2
curve(expr = bernoulli.loglikelihood(x, n = 30, s = 21), 
      from = 0.4, to = 0.9, col = "black", 
      xlab = "theta", ylab = "Funcion de Log-Verosimilitud")
grid()
abline(v = theta.hat, col = "blue", lwd = 2)


#############################################################
# Otra manera de maximizar utilizando la funcion "optimize" #
#############################################################

f = function(x) bernoulli.loglikelihood(x, n = 30, s = 21)

maximizacion <- optimize(f = f, interval = c(0, 1), maximum = TRUE)

# argumento que maximiza la funcion
maximizacion$maximum
# 0.7000058


###################
# Margen de error #
###################

Informacion.Fisher <- -bernoulli.loglikelihood.segundaderivada(theta = 0.7, n = 30, s = 21)
# La informacion de Fisher acerca de la proporcion poblacional de empresas que
# si producen dividendos es 142.8

percentil.z.975 <- qnorm(p = 0.975, mean = 0, sd = 1, lower.tail = TRUE)
# 1.959964

# calculo del margen de error
ME <- percentil.z.975 * sqrt( 1 / Informacion.Fisher )
# 0.1639824

# intervalo de confianza
round(100 * c( theta.hat - ME, theta.hat + ME ), 1)

# Con una confiabilidad del 95%, el margen de error de la estimación de la 
# proporción poblacional de empresas que si producen dividendos es de 16.4%. 
# Por lo tanto, el intervalo de confianza correspondiente es ( 53.6% ; 86.4% ).

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
