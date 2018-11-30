#####################################
# Distribucion de la media muestral #
#####################################

#-------------------------------------------------------------------------------
# EJEMPLO 1
# El peso de los habitantes de una ciudad tiene distribución normal con media 67 
# Kg y desviación típica 5 Kg. 
# Calcular e interpretar la probabilidad de que la media del peso de una muestra 
# aleatoria de veinte personas supere 68.5 Kg.
#-------------------------------------------------------------------------------

# X = Peso (kilogramos), variable aleatoria (poblacion) de estudio.
# mu = 67 (promedio poblacional)
# sigma = 5 (desviacion estandar poblacional)
# X ~ Normal(mu = 67, sigma^2 = 25)
# n = 20 (tamaño de la muestra)
# Se pide calcular Pr(Xbarra > 68.5) = ?

# Solucion:

# En este caso, el tamaño de la muestra no influye en la distribucion de Xbarra
# Xbarra TIENE DISTRIBUCION NORMAL PORQUE LA POBLACION TIENE DISTRIBUCION NORMAL
# Valor esperado de Xbarra = 67
# Desv. Estandar de Xbarra = 5/sqrt(20)
pnorm(q = 68.5, mean = 67, sd = 5/sqrt(20), lower.tail = FALSE)
# 0.08985625
# INTERPRETACION: la probabilidad de que el promedio (muestral) de una muestra 
# aleatoria de 20 personas sea superior a 68.5 Kg es del 8.98%.

# grafico 
windows()
# 1. La poblacion en este ejemplo tiene distribucion normal
curve(dnorm(x, mean = 67, sd = 5), from = 67 - 3*5, to = 67 + 3*5, ylim = c(0,0.35), col = "gray", lwd = 2)
# 2. La distribucion de Xbarra es normal
curve(dnorm(x, mean = 67, sd = 5/sqrt(20)), col = "red", lwd = 2, lty = 2, add = TRUE)

##########################################
# Distribucion de la proporcion muestral #
##########################################

#-------------------------------------------------------------------------------
# El interes recae en los individuos de la poblacion que cumplan una 
# caracteristica determinada. El objeto de estudio es entonces la proporcion 
# (porcentaje) de individuos que satisfagan esa caracteristica de interes.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# EJEMPLO 2
# El Gobierno Nacional asegura que el porcentaje de empresas evasoras de impuestos 
# del país es 3.7%. Para corroborarlo, la oposición planea seleccionar una muestra 
# aleatoria de 200 empresas y examinar su situación tributaria.
# a. Calcular la probabilidad de que la proporción muestral exceda a 5%.
# b. Calcular la probabilidad de que el error muestral correspondiente no exceda a 1%.
#-------------------------------------------------------------------------------

# Caracteristica de interes: la condicion tributaria (evadir impuestos)
# El Gobierno asegura que el valor poblacional es theta = 0.037
# n = 200 (tamaño de la muestra)

# a. Calcular la probabilidad de que la proporción muestral exceda a 5%.
# Se pide calcular Pr(theta.hat > 0.05) = ? donde p es la proporcion muestral.
# En este caso es posible aplicar el teorema del limite central por que 200 >> 30,
# y por tanto la proporcion muestral sigue (aproximadamente) una distribucion 
# normal con media mu = 0.037 y desviacion estandar sigma = sqrt(0.037 * (1 - 0.037) / 200)
pnorm(q = 0.05, mean = 0.037, sd = sqrt(0.037 * (1 - 0.037) / 200), lower.tail = F)
# 0.1650364
# INTERPRETACION: al seleccionar una muestra aleatoria de 200 empresas, la 
# probabilidad de que la proporcion (muestral) de las mismas exceda a 5% es de 16.5%.

# grafico de la aproximacion
windows()
plot(1:20, dbinom(x = 1:20, size = 200, prob = 0.037), pch = 16)
segments(x0 = 1:20, y0 = rep(0,20), x1 = 1:20, y1 = dbinom(x = 1:20, size = 200, prob = 0.037), lwd = 3)
curve(dnorm(x, mean = 200*0.037, sd = 200 * sqrt(0.037 * (1 - 0.037) / 200)), add = TRUE, col = 2, lwd = 2)

# b. Calcular la probabilidad de que el error muestral correspondiente no exceda a 1%.

# El ERROR MUESTRAL se define como la diferencia absoluta entre el estimador y 
# el parametro de interes. 
# Entonces, en este caso el error muestral es | theta.hat - theta |
# Se pide calcular Pr(| theta.hat - theta | <= 0.01) = ?

# Solucion:
# error estandar
sigma.p <- sqrt( 0.037 * (1 - 0.037) / 200 )
# 0.01334747
p1 <- pnorm(q =  0.01/sigma.p, mean = 0, sd = 1, lower.tail = TRUE)
# 0.7731333
p2 <- pnorm(q = -0.01/sigma.p, mean = 0, sd = 1, lower.tail = TRUE)
# 0.2268667
p1 - p2
# 0.5462667

# INTERPRETACION: al seleccionar una muestra aleatoria de 200 empresas, bajo la 
# aseveracion del Gobierno, la probilidad de que el error muestral correspondiente
# no exceda 1% es de tan solo 54.63%.

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------