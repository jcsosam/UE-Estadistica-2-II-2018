################################################################################
#####                      INFERENCIA SOBRE INGRESOS                       #####
################################################################################
################################################################################

#-------------------------------------------------------------------------------
#
# Encuesta Nacional de Presupuestos de los Hogares 2017
#
# Modulo:   Caracteristicas generales personas
#
# Pregunta: Antes de descuentos ¿cuánto ganó el mes pasado? (Incluya propinas y 
#           comisiones, y excluya viáticos y pagos en especie) (P6500)
#
# Identificar cómo manejan sus presupuestos los hogares colombianos.
# Indagar sobre los bienes y servicios adquiridos por los hogares.
# 
# http://microdatos.dane.gov.co/index.php/catalog/566/get_microdata
#
#-------------------------------------------------------------------------------

# importar datos
ingresos <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/ingresos.txt", sep="")


# extraer la variable
y <- ingresos$P6500


################################################################################
#                       Estadistica descriptiva                                #
################################################################################

#///////////////////////////////////////////////////////////////////////////////
#                       Descripción numérica
#///////////////////////////////////////////////////////////////////////////////


# tamaño de la muestra
n <- length(y)
# 59771


# medidas de localizacion
summary(y)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0050  0.6895  0.8000  1.1330  1.2800  9.8710 


# coeficiente de variacion (%)
round(100 * sd(y)/mean(y), 1)
# 84.5


# instalar libreria
#install.packages("DescTools")


# cargar libreria
library(DescTools)


# calculo del coeficiente de Gini
round(100 * Gini(x = y), 1)
# 37.1

# curva de Lorenz
windows()
plot(Lc(x = y), col = "red")
grid()


# porcentaje de personas con ingresos inferiores a dos millones
round(100 * length( y[y < 2] )/n, 2)
# 87.2


# porcentaje de personas con ingresos entre dos y cuatro millones
round(100 * length( y[(2 < y) & (y < 4)] )/n, 2)
# 8.51

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Descripción gráfica
#///////////////////////////////////////////////////////////////////////////////

windows(width = 10, height = 5)
par(mfrow=c(1,2))
# histograma
h <- hist(x = y, freq = FALSE, nclass = 50, ylim = c(0, 2),
     border = "seagreen", col = "mintcream",
     xlab = "Ingresos (Millones)", ylab = "Densidad", main = "")
# diagrama de caja
boxplot(x = y, horizontal = TRUE, 
        boxwex = 0.25, cex = 0.35,
        border = "seagreen", col = "mintcream",
        xlab = "Ingresos (Millones)")
# titulo
title(main = "Encuesta Nacional de Presupuestos de los Hogares 2017", 
      outer = TRUE, line = -2, font.main = 4)


# porcentaje de la categoria mas frecuente 
round(100 * max( h$counts )/n, 2)
# 35.11

#///////////////////////////////////////////////////////////////////////////////


################################################################################
#                       Teorema del Limite Central (1920's)                    #
#                            ( Lindeberg-Lévy )                                #
################################################################################

#///////////////////////////////////////////////////////////////////////////////
#                               INFERENCIA sonbre mu
#///////////////////////////////////////////////////////////////////////////////

#                       mu es el ingreso promedio poblacional                  #

# estimacion
mu.hat.TLC <- mean(y)
# 1.133019


# error estandar
mu.error.estandar.TLC <- sd(y) / sqrt(n)
# 0.003917181


# coeficiente de variación (%)
mu.CV.TLC <- 100 * mu.error.estandar.TLC / mu.hat.TLC
# 0.3457296


# margen de error al 95%
mu.ME.TLC <- qnorm(p = 0.975) * mu.error.estandar.TLC
# 0.007677535


# intervalo de confianza al 95%
mu.IC.TLC <- mu.hat.TLC + c(-1,1) * mu.ME.TLC
# 1.125341 1.140696

#-------------------------------------------------------------------------------
# Nota:
# - El TLC se usa exclusivamente para hacer inferencia sobre promedios.
# - Para usar el TLC no es necesario disponer de un modelo probabilístico.
#-------------------------------------------------------------------------------

#///////////////////////////////////////////////////////////////////////////////


################################################################################
#                       Modelo Gamma: Metodo de los Momentos (1887)            #
#                               ( Pafnuty Chebyshev )                          #
################################################################################

#///////////////////////////////////////////////////////////////////////////////
#                        Estimación de los parametros
#///////////////////////////////////////////////////////////////////////////////

alpha.hat.MOM <- ( mean(y^2) - mean(y) ) / mean(y)
#0.9424753        

beta.hat.MOM  <- ( mean(y^2) - mean(y) ) / mean(y)^2
#0.8318266

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                               Gráfico del modelo
#///////////////////////////////////////////////////////////////////////////////

windows( )
# histograma
hist(x = y, freq = FALSE, nclass = 50,
     border = "seagreen", col = "mintcream",
     xlab = "Ingresos (Millones)", ylab = "Densidad",
     main = "Encuesta Nacional de Presupuestos de los Hogares 2017 \n Modelo Gamma")
# curva
curve(expr = dgamma( x, shape = alpha.hat.MOM , rate = beta.hat.MOM), 
      col = "red", lwd = 2, add = TRUE )
# leyenda
legend("topright", legend = "Método de los Momentos", 
       col = "red", lwd = 2)

#///////////////////////////////////////////////////////////////////////////////


################################################################################
#               Modelo Gamma: Metodo de Maxima Verosimilitud (1920's)          #
#                               ( Ronald Fisher )                              #
################################################################################

#///////////////////////////////////////////////////////////////////////////////
#                       Estimación de los parametros
#///////////////////////////////////////////////////////////////////////////////

#-------------------------------------------------------------------------------
# solucion numerica de la ecuacion usando el paquete rootSolve
# solo es necesario instalar el paquete una vez
#-------------------------------------------------------------------------------

# install.packages("rootSolve") 
library(rootSolve)


# funcion
g <- function(beta) n * log( beta ) - n * digamma( beta * mean(y) ) + sum( log( y ) )

# solucion
solucion.ecuacion <- uniroot(f = g, interval = c(0.001, 10))


beta.hat.MLE  <- solucion.ecuacion$root
# 1.925081

alpha.hat.MLE <- beta.hat.MLE * mean( y )
#  2.181154

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Gráfico de la ecuación y la raiz
#///////////////////////////////////////////////////////////////////////////////

windows( )
#
curve(expr = g(x), from = 1.5, to = 2.5, 
      xlab = expression(beta), ylab = expression(g(beta)), main = " ")
#
abline(h = 0, col = "gray")
#
abline(v = beta.hat.MLE, col = "blue", lty = 2)

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                               Gráfico del modelo
#///////////////////////////////////////////////////////////////////////////////

windows( )
# histograma
hist(x = y, freq = FALSE, nclass = 50,
     border = "seagreen", col = "mintcream",
     xlab = "Ingresos (Millones)", ylab = "Densidad",
     main = "Encuesta Nacional de Presupuestos de los Hogares 2017 \n Modelo Gamma")
# metodo de los maxima verosimilitud
curve(expr = dgamma( x, shape = alpha.hat.MLE , rate = beta.hat.MLE), 
      col = "blue", lwd = 2, add = TRUE )
# metodo de los momentos
curve(expr = dgamma( x, shape = alpha.hat.MOM , rate = beta.hat.MOM), 
      col = "red", lwd = 2, add = TRUE )
# leyenda
legend("topright", 
       legend = c("Método de Máxima Verosimilitud", "Método de los Momentos"),
       col = c("blue","red"), lwd = 2)

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Función de log verosimilitud
#///////////////////////////////////////////////////////////////////////////////

logverosimilitud.GAMMA <- function( alpha, beta ) sum( dgamma(x = y, shape = alpha, rate = beta, log = TRUE) )


# grilla de valores donde evaluar la funcion
eps1 <- 0.02 * alpha.hat.MLE
eps2 <- 0.02 * beta.hat.MLE
a    <- seq( alpha.hat.MLE - eps1, alpha.hat.MLE + eps1, len = 35 )
b    <- seq( beta.hat.MLE  - eps2, beta.hat.MLE  + eps2, len = 35 )


# evaluar la funcion en los valores de la grilla para el grafico 3D
abz  <- data.frame( matrix( NA, length(a)*length(b), 3 ) )
names(abz) <- c( 'a', 'b', 'z' )


# valor que maximiza la funcion de verosimilitud
ce <- abs( logverosimilitud.GAMMA( alpha.hat.MLE, beta.hat.MLE ) )


# evaluacion de la funcion en los puntos de la grilla
k  <- 0
for ( i in 1:length(a) ) {
        for ( j in 1:length(b) ) {
                k <- k + 1
                abz[k, ] <- c( a[i], b[j], exp( logverosimilitud.GAMMA ( a[i], b[j] ) + ce ) )
        }
}

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Gráfico 3D de la verosimilitud
#///////////////////////////////////////////////////////////////////////////////

#install.packages("lattice")
library(lattice)

windows( )
wireframe( z ~ a * b, data = abz,  
           drape = TRUE, aspect = c(0.75,1), colorkey=TRUE,
           xlab = expression(alpha), 
           ylab = expression(beta), 
           zlab = " ",
           main = "Verosimilitud", 
           sub  = "Distribución Gamma",
           col.regions = colorRampPalette(c("yellow", "red"))(100),
           distance = .5, 
           zoom = 0.8,
           scales = list( arrows = TRUE ) )

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Gráfico 3D de la log-varosimilitud
#///////////////////////////////////////////////////////////////////////////////

windows( )
wireframe( log(z) ~ a * b, data = abz,  
           drape = TRUE, aspect = c(1,1), colorkey=TRUE,
           xlab = expression(alpha), 
           ylab = expression(beta), 
           zlab = " ",
           main = "Log-verosimilitud", 
           sub  = "Distribución Gamma",
           col.regions = colorRampPalette(c("yellow", "red"))(100),
           distance = .5, 
           zoom = 0.8,
           scales = list( arrows = TRUE ) )

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Gráfico de contorno de la verosimilitud
#///////////////////////////////////////////////////////////////////////////////

# evaluar la funcion en los valores de la grilla para el grafico de contorno
z <- matrix( data = NA, nrow = length(a), ncol = length(b) )

for ( i in 1:length(a) ) {
        for ( j in 1:length(b) ) {
                z[i, j] <- exp( logverosimilitud.GAMMA ( a[i], b[j] ) + ce )
        }
}

# grafico 1 de contorno de la verosimilitud
windows()
filled.contour(x = a, y = b, z = z,
               col = colorRampPalette(c("yellow", "red"))(20),
               main = "Verosimilitud", 
               sub  = "Distribución Gamma",
               xlab = expression(alpha), 
               ylab = expression(beta),
               plot.axes = { axis(1); axis(2); } )

# grafico 2 de contorno de la verosimilitud
windows()
contour(x = a, y = b, z = z, 
        nlevels = 10, 
        col  = 'blue',
        main = "Verosimilitud", 
        sub  = "Distribución Gamma", 
        xlab = expression(alpha), 
        ylab = expression(beta))

#///////////////////////////////////////////////////////////////////////////////

# criterio del Hessiano para comprobar que hay máximo
n^2 * alpha.hat.MLE * trigamma(alpha.hat.MLE)/beta.hat.MLE^2 - n^2/beta.hat.MLE^2 
# 253508332

-n * trigamma(alpha.hat.MLE)

-n * alpha.hat.MLE / beta.hat.MLE^2

#///////////////////////////////////////////////////////////////////////////////
#                               INFERENCIA
#///////////////////////////////////////////////////////////////////////////////

#-------------------------------------------------------------------------------
# - Maximización usando la función "optim". Esta función permite obtener los 
#   valores que maximizan la función junto con la matriz Hessiana.
# - La función optim esta diseñada para minimizar funciones.
#-------------------------------------------------------------------------------

# reciproco de la log-verosimilitud
menos.logverosimilitud.GAMMA <- function( pars ) -logverosimilitud.GAMMA( pars[1], pars[2] )


# optimizacion
temp <- optim(par = c(alpha.hat.MOM, beta.hat.MOM ), 
              fn = menos.logverosimilitud.GAMMA, 
              hessian = TRUE)


# estimacion
# observe que la solucion coincide con con alpha.hat.MLE y beta.hat.MLE
temp$par


# matriz observada de fisher
# no es necesario anteponer el menos porque ya se hizo al definir la funcion menos.logverosimilitud.GAMMA
I.hat <- temp$hessian

# otra forma
I.hat <- matrix( c(n*trigamma(alpha.hat.MLE), -n/beta.hat.MLE, 
                  -n/beta.hat.MLE, n*alpha.hat.MLE/beta.hat.MLE^2 ), nrow = 2, ncol = 2)

# infomacion de fisher inversa (matriz de varianza y covarianza)
solve(I.hat)

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                    Inferencia sobre mu = alpha/beta
#///////////////////////////////////////////////////////////////////////////////

# estimacion puntual de mu
mu.hat.MLE <- alpha.hat.MLE / beta.hat.MLE


# error estandar 
# es indispensable usar el metodo delta
gradiente <- as.matrix( c( 1/beta.hat.MLE, -alpha.hat.MLE/beta.hat.MLE^2 ) )

mu.error.estandar.MLE <- as.numeric( sqrt( t( gradiente ) %*% solve( I.hat ) %*% gradiente ) )
# es necesario usar "as.numeric" porque el resultado es una matriz de 1 x 1


# coeficiente de variación (%)
mu.CV.MLE <- 100 * mu.error.estandar.MLE / mu.hat.MLE


# margen de error
mu.ME.MLE <- qnorm(p = 0.975) * mu.error.estandar.MLE


# intervalo de confianza al 95%
mu.IC.MLE <- mu.hat.MLE + c(-1,1) * qnorm(p = 0.975) * mu.error.estandar.MLE

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                    Inferencia sobre sigma = raiz(alpha)/beta
#///////////////////////////////////////////////////////////////////////////////

# estimacion puntual de sigma
sigma.hat.MLE <- sqrt(alpha.hat.MLE) / beta.hat.MLE


# error estandar
gradiente <- as.matrix( c( (1/2)*alpha.hat.MLE^(-1/2)/beta.hat.MLE, -sqrt(alpha.hat.MLE)/beta.hat.MLE^2 ) )


sigma.error.estandar.MLE <- as.numeric( sqrt( t( gradiente ) %*% solve( I.hat ) %*% gradiente ) )
# es necesario usar "as.numeric" porque el resultado es una matriz de 1 x 1


# coeficiente de variación (%)
sigma.CV.MLE <- 100 * sigma.error.estandar.MLE / sigma.hat.MLE


# margen de error
sigma.ME.MLE <- qnorm(p = 0.975) * sigma.error.estandar.MLE


# intervalo de confianza al 95%
sigma.IC.MLE <- sigma.hat.MLE + c(-1,1) * qnorm(p = 0.975) * sigma.error.estandar.MLE

#///////////////////////////////////////////////////////////////////////////////


################################################################################
#                          Modelo Gamma: Bootstrap (1980's)                    #
#                               ( Eric Efron )                                 #
################################################################################

#///////////////////////////////////////////////////////////////////////////////
#                               proceso de remuestreo
#///////////////////////////////////////////////////////////////////////////////

# numero de muestras de la muestra
M <- 20000


# para almacenar los promedios y las desviaciones estandar
prom.bootstrap <- matrix(NA, nrow = M, ncol = 1)
desv.bootstrap <- matrix(NA, nrow = M, ncol = 1)


# semilla de la simulacion : la semilla se utiliza para que todos los que ejecuten
# la simulacion tengan los mismos resultados. El numero "1234" es arbitrario.
set.seed(seed = 1234)

# remuestreo
for (i in 1:M) {
        muestra <- sample(x = y, size = n, replace = TRUE)
        prom.bootstrap[i] <- mean( muestra )
        desv.bootstrap[i] <- sd( muestra )
        rm(muestra)
}

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Inferencia sobre mu
#///////////////////////////////////////////////////////////////////////////////

# estimacion puntual de mu
mu.hat.BOOT <- mean( prom.bootstrap )


# error estandar
mu.error.estandar.BOOT <- sd( prom.bootstrap )


# coeficiente de variación (%)
mu.CV.BOOT <- 100 * mu.error.estandar.BOOT / mu.hat.BOOT


# intervalo de confianza al 95%
mu.IC.BOOT <- quantile(x = prom.bootstrap, probs = c(0.025, 0.975))


# margen de error
mu.q1.BOOT <- quantile(x = prom.bootstrap, probs = 0.025)
mu.q3.BOOT <- quantile(x = prom.bootstrap, probs = 0.975)

mu.ME.BOOT <- as.numeric( (mu.q3.BOOT - mu.q1.BOOT)/2 )


# grafico
windows()
# histograma
hist(x = prom.bootstrap, freq = FALSE, col = "gray100",
     main = "Promedio Bootstrap", xlab = "Promedio", ylab = "Densidad")
# curva
lines(density(prom.bootstrap), col = "gray", lwd = 2)
# lineas
abline(v = mu.hat.BOOT, col = "red",  lwd = 2, lty = 2)
abline(v = mu.q1.BOOT,  col = "blue", lwd = 2, lty = 3)
abline(v = mu.q3.BOOT,  col = "blue", lwd = 2, lty = 3)

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                       Inferencia sobre sigma
#///////////////////////////////////////////////////////////////////////////////

# estimacion puntul de sigma
sigma.hat.BOOT <- mean( desv.bootstrap )


# error estandar
sigma.error.estandar.BOOT <- sd( desv.bootstrap )


# coeficiente de variación (%)
sigma.CV.BOOT <- 100 * sigma.error.estandar.BOOT / sigma.hat.BOOT


# intervalo de confianza al 95%
sigma.IC.BOOT <- quantile(x = desv.bootstrap, probs = c(0.025, 0.975))


# margen de error
sigma.q1.BOOT <- quantile(x = desv.bootstrap, probs = 0.025)
sigma.q3.BOOT <- quantile(x = desv.bootstrap, probs = 0.975)

sigma.ME.BOOT <- as.numeric( (sigma.q3.BOOT - sigma.q1.BOOT)/2 )


# grafico
windows()
# histograma
hist(x = desv.bootstrap, freq = FALSE, col = "gray100",
     main = "Desviación Estándar Bootstrap", xlab = "Promedio", ylab = "Densidad")
# curva
lines(density(desv.bootstrap), col = "gray", lwd = 2)
# lineas
abline(v = sigma.hat.BOOT, col = "red",  lwd = 2, lty = 2)
abline(v = sigma.q1.BOOT,  col = "blue", lwd = 2, lty = 3)
abline(v = sigma.q3.BOOT,  col = "blue", lwd = 2, lty = 3)

#///////////////////////////////////////////////////////////////////////////////


################################################################################
#                       Tabla de resultados                                    #
################################################################################

#///////////////////////////////////////////////////////////////////////////////

tabla.mu <- rbind(c(mu.hat.TLC,  mu.error.estandar.TLC,  mu.CV.TLC,  mu.ME.TLC,  mu.IC.TLC),
                  c(mu.hat.MLE,  mu.error.estandar.MLE,  mu.CV.MLE,  mu.ME.MLE,  mu.IC.MLE),
                  c(mu.hat.BOOT, mu.error.estandar.BOOT, mu.CV.BOOT, mu.ME.BOOT, mu.IC.BOOT))

rownames(tabla.mu) <- c("TLC", "MLE", "BOOT")
colnames(tabla.mu) <- c("Estimación", "Error Estándar", "CV (%)", "Margen de Error", "Inf. 95% ", "sup. 95% ")

round(tabla.mu, 4)

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////

tabla.sigma <- rbind(c(sigma.hat.MLE,  sigma.error.estandar.MLE,  sigma.CV.MLE,  sigma.ME.MLE,  sigma.IC.MLE),
                     c(sigma.hat.BOOT, sigma.error.estandar.BOOT, sigma.CV.BOOT, sigma.ME.BOOT, sigma.IC.BOOT))

rownames(tabla.sigma) <- c("MLE", "BOOT")
colnames(tabla.sigma) <- c("Estimación", "Error Estándar", "CV (%)", "Margen de Error", "Inf. 95% ", "sup. 95% ")

round(tabla.sigma, 4)

#///////////////////////////////////////////////////////////////////////////////


################################################################################
#                       Bondad de ajuste del modelo Gamma                      #
################################################################################

#///////////////////////////////////////////////////////////////////////////////
#                               Simulación
#///////////////////////////////////////////////////////////////////////////////

# numero de muestras de la simulacion
M <- 10000


# para almacenar los promedios y las desviaciones estandar
prom.simulacion <- matrix(NA, nrow = M, ncol = 1)
desv.simulacion <- matrix(NA, nrow = M, ncol = 1)


# semilla de la simulacion : la semilla se utiliza para que todos los que ejecuten
# la simulacion tengan los mismos resultados. El numero "1234" es arbitrario.
set.seed(seed = 1234)

# simulacion
for (i in 1:M) {
        muestra <- rgamma(n = n, shape = alpha.hat.MLE, rate = beta.hat.MLE)
        prom.simulacion[i] <- mean( muestra )
        desv.simulacion[i] <- sd( muestra )
        rm(muestra)
}

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                               Gráfico para el promedio
#///////////////////////////////////////////////////////////////////////////////

windows()
# histograma
hist(x = prom.simulacion, freq = FALSE, col = "gray100",
     main = "Bondad de ajuste del promedio", xlab = "Promedio", ylab = "Densidad")
# curva
lines(density(prom.simulacion), col = "gray", lwd = 2)
# lineas
abline(v = mean(y), col = "red", lwd = 2, lty = 2)

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
#                               Gráfico para la desviación
#///////////////////////////////////////////////////////////////////////////////

windows()
# histograma
hist(x = desv.simulacion, freq = FALSE, col = "gray100",
     main = "Bondad de ajuste de la desviación estándar", xlab = "Promedio", ylab = "Densidad")
# curva
lines(density(desv.simulacion), col = "gray", lwd = 2)
# lineas
abline(v = sd(y), col = "red", lwd = 2, lty = 2)

#///////////////////////////////////////////////////////////////////////////////