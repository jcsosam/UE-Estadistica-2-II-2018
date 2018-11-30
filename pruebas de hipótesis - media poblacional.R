#############################################
### PRUEBAS DE HIPOTESIS MEDIA POLACIONAL ###
#############################################

# importar PROM
PROM <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/PROM.txt", sep="")

# vizualizar PROM
View(PROM)

# DESCRIPCION :
# PROM : Calificación promedio.
#
# OBJETIVO :
# Comprobar por medio de pruebas de hipotesis si la media de la calificación 
# promedio difiere de 4.0.

# X = Calificación promedio (variable de interés).

# PASO 1:
# El sistema de hipotesis es H0: mu = 4 frente a H1: mu != 4
# El valor hipotetico es mu0 = 4.
# Con un nivel de significancia de alpha = 0.05

mu0   = 4

alpha = 0.05

# PASO 2: 
# Calculo del estadistico de prueba t

x  = PROM$promedio     # datos (calificaciones promedio)
n  = length(x)         # 100 estudiantes.
xb = mean(x)           # 3.7543 promedio muestral (x barra).
s  = sd(x)             # 0.4792634 desv. estandar muestral. 

#-------------------------------------------------------------------------------
# Margen de error
ME <- 1.96 * s / sqrt(n)  # 0.09393563

# Intervalo de confianza
c( xb - ME, xb + ME )  #  3.660364 3.848236

# Se estima que la media de las calificaciones promedio es de 3.7 con un margen 
# de error de 0.094 puntos con una confiabilidad del 95%, esto es, con una 
# confiabilidad del 95%, la media de las calificaciones promedio de los estudiantes 
# de esta Facultad se encuentra entre 3.66 y 3.84.
#
# Este resultado implica que la media de las calificaciones promedio difiere 
# significativamente (es significativamente inferior a) de 4.0.
#-------------------------------------------------------------------------------

est.prueba = (xb - mu0) / (s/sqrt(n))
# el estadistico de prueba calculado es t = -5.126617

# PASO 3: 
# estalecer la region critica (percentil 95 distr t con 99 grados de libertad)
percentil.t = qt(p = 1 - alpha/2, df = n - 1, lower.tail = TRUE)
# 1.984217

#el percentil de la distr normal es un aproximacion ya que n >= 30
#qnorm(p = 1 - alpha/2, mean = 0, sd = 1, lower.tail = TRUE)
# 1.959964

# La region critica esta compuesta de todos los valores absolutos superiores a 1.98.
# La region critica es RC = { x : |x| > 1.98 }

# CALCULO del valor p
# Se debe "multiplicar por 2" porque es una prueba bilateral
valor.p = 2 * pt(q = abs(est.prueba), df = n - 1, lower.tail = FALSE)
# 1.464286e-06

# aproximación con la distribución normal
#2 * pnorm(q = abs(est.prueba), mean = 0, sd = 1, lower.tail = FALSE)
#2.949951e-07

# PASO 4:
# tomar la desicion: 
# Como el estadistico de prueba t = -5.126617 pertenece a la region critica (RC)
# ya que |-5.126617| > 1.984217, entonces se rechaza la hipotesis nula (H0).

# El valor p = 1.464286e-06 indica que se debe rechazar la hipotesis nula (H0) 
# ya que p < 0.05.

# PASO 5:
# interpretación:
# Como se ha rechazo la hipotesis nula, entonces se concluye que hay
# suficiente evidencia en la muestra para establacer la media de la calificación 
# promedio de todos los etudiantes de la facultad difiere significativamente de 4.0


# grafico de la region de rechazo
windows()
# curva
curve(expr = dt(x, df =  n - 1), from = -6, to = 6, col = "black", lwd = 2, xlab = "x", ylab = "Densidad t")
# area bajo la curva cola inferior
grilla <- seq(from = -6, to = -percentil.t, by = 0.01)
cord.x <- c(-6, grilla, -percentil.t)
cord.y <- c(0, dt(x = grilla, df = n - 1), 0)
polygon(cord.x,cord.y,col='orange', density = 30)
# area bajo la curva cola superior
grilla <- seq(from = percentil.t, to = 6, by = 0.01)
cord.x <- c(percentil.t, grilla, 6)
cord.y <- c(0, dt(x = grilla, df = n - 1), 0)
polygon(cord.x,cord.y,col='orange', density = 30)
# lineas
abline(v = 0, lty = 2, col = "gray")
abline(v = c(-percentil.t, percentil.t), col = "red")
abline(v = est.prueba, col = "blue", lwd = 2)
# leyenda
legend("topright", 
       legend = c("Estadístco de prueba", "Percentiles", "Región Crítica"), 
       fill = c("blue", "red","orange"), border = c("blue", "red","orange"), bg = "white")

########################################
#### Alternativa de calculo usando R ###
########################################

t.test(x = x, alternative = "two.sided", mu = mu0, conf.level = 1 - alpha)

#One Sample t-test
#
#data:  x
#t = -5.1266, df = 99, p-value = 1.464e-06
#alternative hypothesis: true mean is not equal to 4
#95 percent confidence interval:
#        3.659204 3.849396
#sample estimates:
#        mean of x 
#3.7543

########################################
### Validar los supuestos del modelo ###
########################################

# medidas de localizacion
summary(x)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.590   3.417   3.720   3.754   4.075   4.970 


# descripción gráfica
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
# histograma
curve(expr = dnorm(x, mean = xb, sd = s), from = xb - 3*s, to = xb + 3*s, 
      col = "red", xlab = "Salario (millones)", ylab = "Densidad", main = "Histograma")
hist(x, freq = FALSE, add = TRUE)
# qqplot
qqnorm(x, main = "Gráfico Normal cuantíl-cuantíl", xlab = "Cuantiles Teoricos", 
       ylab = "Cuantiles de la muestra")
qqline(x, col = "red")

        
#install.packages(car)
library(car)

windows()
qqPlot(x = x, main = "Gráfico Normal cuantíl-cuantíl", xlab = "Cuantiles Teoricos", 
       ylab = "Cuantiles de la muestra")



# simulacion
grafico.original <- qqnorm(x)
windows()
plot(x = grafico.original$x, y = grafico.original$y, xlab = "Cuantiles Teoricos",
     ylab = "Cuantiles de la muestra")
for (i in 1:1000) {
     muestra.simulacion <- rnorm(n = n, mean = xb, sd = s)
     grafico.simulacion <- qqnorm(muestra.simulacion, plot.it = FALSE)
     lines(x = grafico.simulacion$x, y = grafico.simulacion$y, type = "p", col = "lightgray")
}
lines(x = grafico.original$x, y = grafico.original$y, col = "blue", type = "p")
qqline(x, col = "red")



# prueba de normalidad
shapiro.test(x)
#Shapiro-Wilk normality test
#
#data:  x
#W = 0.9921, p-value = 0.8284

# Interpretación:
# Dado que el valor p (0.8284) es mayor que 5%, entonces no hay evidencia en la 
# muestra para rechazar la hipotesis nula (H0), es decir, no existen indicios 
# en la muestra para dudar de la normalidad de las calificaciones promedio de los
# estudiantes.

##########################
### Poder de la prueba ###
##########################

power.test <- function(datos, mu0, mu1, alpha) {
     n  <- length(datos) 
     xb <- mean(datos)
     s  <- sd(datos)
     z  <- qnorm(p = 1 - alpha)
     A  <- mu0 + z * s/sqrt(n)
     B  <- mu0 - z * s/sqrt(n)
     yA <- (A - mu1) / (s/sqrt(n))
     yB <- (B - mu1) / (s/sqrt(n))
     BETA <- pnorm( q = yA, mean = 0, sd = 1, lower.tail = TRUE ) -
             pnorm( q = yB, mean = 0, sd = 1, lower.tail = TRUE )
     return(1 - BETA)
}

f <- function(mu1) power.test(datos = x, mu0 = 4, mu1, alpha = 0.05) 

f(mu1 = 4.01)
# 0.1073776
# La potencia de la prueba cuando mu = 4.01 es 0.1073776 (es decir la probabilidad del
# error tipo II es 1 - 0.1073776). Cuando el valor verdadero de mu es 4.01, solo el 10.7% de
# las veces el test indica la decision correcta.

f(mu1 = 4.05)
# 0.2773176

# grafico de la potencia
windows(width = 10, height = 10)
# grafico 1
curve(expr = f, from = 3.5, to = 4.5, col = "blue", main = "Poder de la prueba",
      xlab = expression(mu), ylab = "Poder", lwd = 2)
grid()


g <- function(x) 1 - f(x)

# grafico del error tipo 2
windows(width = 10, height = 10)
# grafico 1
curve(expr = g, from = 3.5, to = 4.5, col = "blue", main = "Error 2 de la prueba",
      xlab = expression(mu), ylab = "Error", lwd = 2)
grid()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------