###########################################################################
### EJEMPLO: PRUEBAS DE HIPOTESIS DIFERENCIA DE PROPORCIONES POLACIONAL ###
###########################################################################


# importar ESTEC


# vizualizar ESTEC
View(ESTEC)

# DESCRIPCION :
# UNIV : universidad.
# OP   : opinion (0 = "no", 1 = "sí").
#
# OBJETIVO :
# Comprobar por medio de pruebas de hipotesis si hay diferencias significativas
# entre la proporción de estudiantes a favor en las universidades.

# caracteristica de interes: estar a favor de la medida economica.

# PASO 1:
# El sistema de hipotesis es H0: pi1 - pi2 = 0 frente a H1: pi1 - pi2 != 0
# El valor hipotetico es delta0 = 0.
# Con un nivel de significancia de alpha = 0.05

alpha = 0.05

delta0 = 0

# PASO 2: 
# Calculo del estadistico de prueba z

attach(ESTEC)

x = OP[ UNIV == 1 ]  # opnion universidad 1
y = OP[ UNIV == 2 ]  # opnion universidad 2

n1 = length(x)  # 100 estudiantes universidad 1
n2 = length(y)  # 100 estudiantes unviersidad 2

pi1.est = sum(x)/n1  # 29% esta a favor universidad 1 (proporcion muestral)
pi2.est = sum(y)/n2  # 38% esta a favor universidad 2 (proporcion muestral)

pi.est  = ( sum(x) + sum(y) ) / (n1 + n2) 
# 33.5% esta a favor en ambas universidades (proporcional grobal muestral)

est.prueba = ( pi1.est - pi2.est - delta0 ) / sqrt( pi.est * (1-pi.est) * (1/n1 + 1/n2) )
# el estadistico de prueba calculado es z = -1.348324

# PASO 3: 
# estalecer la region critica
percentil.z1 = qnorm(p = alpha/2, mean = 0, sd = 1, lower.tail = TRUE)
#-1.959964

percentil.z2 = qnorm(p = 1-alpha/2,mean = 0, sd = 1, lower.tail = TRUE)
#1.959964

# La region critica esta compuesta de todos los valores inferiores a -1.96
# o superiores a superiores a 1.96.
# La region critica es RC = { x : x < -1.96 o x > 1.96 }
#                      RC = { x : |x| > 1.96}

# Calculo del valor p
valor.p = 2 * pnorm(q = est.prueba, mean = 0, sd = 1, lower.tail = TRUE)
# El valor p = 0.1775542, lo cual indica que NO se debe rechazar la hipotesis 
# nula (H0).

# PASO 4:
# tomar la desicion: 
# Como el estadistico de prueba z = -1.34 no pertenece a la region critica (RC)
# ya que -1.96 < -1.34 < 1.96, entonces NO se rechaza la hipotesis nula (H0).

# PASO 5:
# interpretación:
# Como no se ha rechazo la hipotesis nula, entonces se concluye que no hay
# suficiente evidencia en la muestra para establacer que hay diferencias 
# significativas entre las proporciones de estudiantes a favor de la medida
# economica en las dos universidades.

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# estimacion puntual
pi1.est - pi2.est
# -0.09

# margen de error
ME <- 1.96 * sqrt( pi1.est * (1 - pi1.est) / n1 + pi2.est * (1 - pi2.est) / n2 )
# 0.1302331
# El margen de error para la diferencia de proporciones es del 13%. 

# intervalo de confianza
c( pi1.est - pi2.est - ME, pi1.est - pi2.est + ME )
# -0.22023311  0.04023311
# Con una confiabilidad del 95%, se concluye que la diferencia de proporciones 
# poblacional de estudiantes a favor de la medida entre ambas universidades esta
# entre - 22% y 4%. Entonces se concluye que no hay
# suficiente evidencia en la muestra para establacer que hay diferencias 
# significativas entre las proporciones de estudiantes a favor de la medida
# economica en las dos universidades.
