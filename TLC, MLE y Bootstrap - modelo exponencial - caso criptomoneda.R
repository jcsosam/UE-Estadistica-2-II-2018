################################################
#####     EJ. DISTRIBUCION EXPONENCIAL     #####
################################################

# Tiempos (velocidad) de transacciones de una moneda virtual (minutos)

# Ranking the Average Transaction Speeds of the 15 Largest Cryptocurrencies
# https://www.fool.com/investing/2018/05/23/ranking-the-average-transaction-speeds-of-the-15-l.aspx

#-------------------------------------------------------------------------------
# objetivo: hacer inferencia sobre el tiempo promedio entre transacciones.
#-------------------------------------------------------------------------------

# importar datos
BT <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/BT.txt", sep="")

# adjuntar base de datos
attach(BT)


##################################
# Analisis exploratorio de datos #
##################################

######## medidas estadisticas

summary(Time)

round(100 * sd(Time)/mean(Time), 1)


######## graficos

windows(width = 10, height = 5)
par(mfrow = c(1,2))
# histograma
hist(x = Time, freq = FALSE, border = "red", col = "mistyrose", density = 15,
     xlab = "Tiempo", ylab = "Densidad", main = "Histrograma")
# diagrama de caja
boxplot(x = Time, horizontal = TRUE, border = "red", col = "mistyrose", 
        boxwex = 0.35, cex = 0.5, 
        xlab = "Tiempo", main = "Diagrama de caja")

#-------------------------------------------------------------------------------
# Dada la distriución empirica de los datos la DISTRIBUCION EXPONENCIAL parece
# ser un buen modelo para estimar el tiempo promedio de transacciones
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


##############################
# Teorema del límite central #
##############################

# tamaño de la muestra
n <- length( Time )


# promedio (estimacion)
xbar <- mean( Time )
# 0.526065

# desviacion estandar muestral (s)
desv <- sd( Time )
# 0.5294476

# error estandar
round(desv / sqrt(n), 3)
# 0.014

# coeficiente de variación
round(100 * (desv / sqrt(n)) / xbar, 1)
# 2.6%
# baja dispersion respecto a la estimación (promedio)
# las estimacion son homogeneas

# intervalo de confianza
round( xbar + c(-1,1) * qnorm(p = 0.975) * ( desv / sqrt(n) ), 3)
# 0.500 0.553

#--------------------------------------------------------------------------------
# con una confiabilidad del 95%, se concluye que el tiempo promedio (poblacional)
# entre transacciones de la criptomoneda se encuentra entre 0.500 y 0.553 minutos. 
#-------------------------------------------------------------------------------

########################
# Maxima verosimilitud #
########################

# valor del estadistico suficiente  (suma)
s <- sum(Time) 


######## funciones 

# funcion de log-verosimilitud
exponential.loglikelihood <- function( lambda, n, s ) {
        
        return( n * log(lambda) - lambda * s )
        
}

# funcion de verosimilitud
exponential.likelihood <- function( lambda, n, s ) {
        
        return( exp( exponential.loglikelihood( lambda, n, s ) ) )
        
}
        
# primera derivada de log-verosimilitud
exponential.loglikelihood.primeraderivada <- function( lambda, n, s ) 
{
        
        return( n/lambda - s )
        
}

# segunda derivada de la log-verosimilitud
exponential.loglikelihood.segundaderivada <- function( lambda, n, s ) 
{
        
        return( -n/lambda^2 )
        
}


######## estimación puntual

lambda.hat <- 1 / mean(Time)


######## Otra manera de maximizar utilizando la funcion "optimize"

f = function(x) exponential.loglikelihood(x, n = n, s = s)

maximizacion <- optimize(f = f, interval = c(0, max(Time)), maximum = TRUE)

# argumento que maximiza la funcion
maximizacion$maximum


######## grafico verosimiliud y logverosimilitud

windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# grafico 1
curve(expr = exponential.likelihood(x, n = n, s = s), 
      from = 1.5, to = 2.5, col = "black", 
      xlab = "lambda", ylab = "Funcion de Verosimilitud")
grid()
abline(v = lambda.hat, col = "blue", lwd = 2, lty = 2)
# grafico de la log-verosimilitud
curve(expr = exponential.loglikelihood(x, n = n, s = s), 
      from = 1.5, to = 2.5, col = "black", 
      xlab = "lambda", ylab = "Funcion de Log-Verosimilitud")
grid()
abline(v = lambda.hat, col = "blue", lwd = 2, lty = 2)


######## grafico de la derivada y de la segunda derivada

windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# primera
curve(expr = exponential.loglikelihood.primeraderivada(x, n = n, s = s), 
      from = 1.5, to = 2.5, col = "black", 
      xlab = expression(lambda), ylab = "Derivada")
grid()
abline(v = lambda.hat, col = "blue", lwd = 2)
# segunda
curve(expr = exponential.loglikelihood.segundaderivada(x, n = n, s = s), 
      from = 1.5, to = 2.5, col = "black", 
      xlab = expression(lambda), ylab = "Segunda derivada")
grid()
abline(v = lambda.hat, col = "blue", lwd = 2)


# información de Fisher
Informacion.Fisher <- -exponential.loglikelihood.segundaderivada(lambda = lambda.hat, n = n, s = s)


# error estandar (lambda)
round(sqrt(1 / Informacion.Fisher ), 3)


# coeficiente de variación (lambda)
round(100 * sqrt(1 / Informacion.Fisher )/lambda.hat, 1)


# calculo del margen de error (lambda)
ME <- qnorm(p = 0.975) * sqrt( 1 / Informacion.Fisher )


# intervalo de confianza (lambda)
round(c( lambda.hat - ME, lambda.hat + ME ), 3)


# estimación del tiempo medio (1/lambda)
round(1 / lambda.hat, 3)


# error estandar (1/lambda)
round(1/(sqrt(n)*lambda.hat), 3)


# margen de error (1/lambda) usando el metodo delta
round(qnorm(p = 0.975) * (1/(sqrt(n) * lambda.hat)), 3)


# intervalo de confianza tiempo medio (1/lambda)
round( 1/lambda.hat + c(-1,1) * qnorm(p = 0.975) * (1/(sqrt(n)*lambda.hat)), 3)


#############
# Bootstrap #
#############

# numero de muestras (bootstrap)
M <- 50000

# almacenar los promedios
promedio.bootstrap <- matrix(NA, nrow = M, ncol = 1)

# semilla de la simulacion : la semilla se utiliza para que todos los que ejecuten
# la simulacion tengan los mismos resultados. El numero "1234" es arbitrario.
set.seed(seed = 1234)

# simulacion
for (i in 1:M) {
        muestra <- sample(x = Time, size = n, replace = TRUE)
        promedio.bootstrap[i] <- mean( muestra )
        rm(muestra)
}

# estimacion Bootstrap
round(mean(promedio.bootstrap), 3)


# error estandar
round(sd(promedio.bootstrap), 3)


# intervalo de confianza Bootstrap
round(quantile(x = promedio.bootstrap, probs = c(0.025, 0.975)), 3)

# grafico
windows()
hist(x = promedio.bootstrap, freq = FALSE, main = " ")
abline(v = quantile(x = promedio.bootstrap, probs = c(0.025, 0.975)), col = 2, lty = 2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------