##########################
# LA DISTRIBUCION NORMAL #
##########################

#-------------------------------------------------------------------------------
# Algunas propiedades:
# - Forma de campana (simetrica).
# - Cuanto mas grande es la variabilidad, la curva es mas "aplanada".
# - Media = Moda = Mediana.
# - Mas del 99% del area bajo la curva esta entre mu - 3*sigma y mu + 3*sigma
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Caso (ej. de probilidades usando la distribucion Normal)
# La distribución de los ingresos diarios (en miles de pesos) de los empleados
# de un sector de la ciudad tienen distribución normal con media cien mil pesos 
# y desviación estándar dos mil pesos.
#-------------------------------------------------------------------------------

# Graficar la distribucion de los ingresos (graficar la poblacion).

# Solucion:
# X = ingresos diarios (en miles de pesos), variable aleatoria (poblacion) de interes.
# X ~ Normal(mu = 100, sigma^2 = 4) 

windows()
curve(expr = dnorm(x, mean = 100, sd = 2), from = 88, to = 112, lwd = 2, lty = 1,
      xlab = "Ingresos diarios (en miles de $)", ylab = "Funcion de densidad",
      main = "Distribucion normal")
curve(expr = dnorm(x, mean = 100, sd = 3), from = 88, to = 112, lwd = 2, lty = 2,
      col = "blue", add = TRUE)
legend("topright", legend = c("sd = 2","sd = 3"), col = c("black","blue"), 
       lty = c(1,2),lwd = 2)
grid()

# Qué porcentaje de empleados gana menos de $98.500?
# En simbolos: Pr(X < 98.5) = ?

pnorm(q = 98.5, mean = 100, sd = 2, lower.tail = TRUE)
# 0.2266274
# El 22.67% de los empleados en la poblacion ganan menos de $98,500 diarios.

# Qué porcentaje de empleados gana entre $99.000 y $102.500?
# Pr(99 < X < 102.5) = ?

# Area bajo la curva antes de 99
p1 <- pnorm(q = 99, mean = 100, sd = 2, lower.tail = TRUE) 
# 0.3085375
# Area bajo la curva despues de 102.5
p2 <- pnorm(q = 102.5, mean = 100, sd = 2, lower.tail = FALSE) 
# 0.1056498
1 - (p1 + p2)
# 0.5858127
# El 58.58% de los empleados gana entre 99,000 y 102,500 diarios.

# Otra forma
p3 <- pnorm(q = 102.5, mean = 100, sd = 2, lower.tail = TRUE) 
p3 - p1
# 0.5858127

# Cuál es el salario que separa el 5% de los empleados mejor pagos del resto de 
# los empleados?
# Se pide calcular el percentil 95: Pr(X > a) = 0.05 o Pr(X < a) = 0.95.
qnorm(p = 0.05, mean = 100, sd = 2, lower.tail = FALSE)
# 103.2897
qnorm(p = 0.95, mean = 100, sd = 2, lower.tail = TRUE)
# 103.2897
# El salario diario que separa el 5% de los empleados mejor pagos del resto es 
# $103,280.

# Si hay un aumento del 10% y un subsidio de 10 mil pesos, cuál es la 
# probabilidad de que un empleado gane más de $118.500?
# Y = salario diario (en miles de pesos) CON EL AUMENTO MAS EL SUBSIDIO.
# Y = (1 + 0.1) * X + 10 = 1.1*X + 10

# Media (valor esperado) de Y
mu.Y = 1.1*100 + 10
mu.Y
# 120
# El valor esperado (poblacional) de los ingresos con el aumento y el subsudio
# es $120,000 diarios.

# Desv. estandar de Y
sd.Y = sqrt( 1.1^2 * 4 )
sd.Y
# 2.2
# La desviacion estandar (poblacional) de los ingresos con el aumento y el 
# subsudio es $2,200 diarios.

# Dado que X tiene distribucion normal, entonces Y TAMBIEN TIENE DISTRIBUCION 
# NORMAL y por lo tanto Y ~ Normal(mu = 120, sigma^2 = 2.2^2) 

# Pr(Y > 118.5) = ?
pnorm(q = 118.5, mean = 120, sd = 2.2, lower.tail = FALSE)
# 0.752323
# El 75.23% de los empleados en la poblacion ganan mas de $118,500 teniendo
# en cuenta el aumento mas el subsidio.

# Pr(X > 118.5) = ?
pnorm(q = 118.5, mean = 100, sd = 2, lower.tail = FALSE)
# 1.122463e-20

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------