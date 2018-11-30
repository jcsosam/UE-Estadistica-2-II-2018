###########################################################
### EJEMPLO: PRUEBAS DE HIPOTESIS PROPORCION POLACIONAL ###
###########################################################

# importar BAYVIEW
BAYVIEW <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/BAYVIEW.txt")

# vizualizar BAYVIEW
View(BAYVIEW)

# DESCRIPCION :
# CI  : copió de internet
# CE  : copió en un examen
# CPI : copió en un trabajo (proyecto) individual
# GEN : genero
#
# OBJETIVO :
# Comprobar por medio de pruebas de hipotesis si la proporción poblacional de 
# estudiantes que hizo alguna clase de trampa es inferior a 56%

# caracteristica de interes: cometer fraude (trampa)

# PASO 1:
# El sistema de hipotesis es H0: pi >= 0.56  frente a H1: pi < 0.56 (56%)
# El valor hipotetico es pi0 = 0.56.
# Con un nivel de significancia de alpha = 0.05

pi0   = 0.56

alpha = 0.05

# PASO 2: 
# Calculo del estadistico de prueba z

attach(BAYVIEW)

x <- as.numeric( CI == "Yes" | CE == "Yes" | CPI == "Yes" )
n <- length(x)
# 90 estudiantes (tamaño de la muestra)

pi.hat <- sum(x) / n
# 0.4111111
# se estima que el porcentaje de estudiantes que cometieron alguna clase de plagio
# es de 41.11%

est.prueba = ( pi.hat - pi0 ) / sqrt( pi0 * (1-pi0) / n )
# el estadistico de prueba es z = -2.84553

# PASO 3: 
# estalecer la region critica
percentil.z = qnorm(p = alpha, mean = 0, sd = 1, lower.tail = TRUE)
#-1.644854

# La region critica esta compuesta por todos los valores inferiores a -1.64.
# La region critica es RC = { x : x < -1.64 }

# CALCULO del valor p
valor.p = pnorm(q = est.prueba, mean = 0, sd = 1, lower.tail = TRUE)
#0.002216878
# El valor p es 0.22%.

# PASO 4:
# Tomar la desicion: 
# Como el estadistico de prueba z = -2.84 sí pertenece a la region critica (RC)
# ya que -2.84 < -1.64, entonces se rechaza la hipotesis nula (H0).
# Equivalentemente, el valor p (0.22%) indica que se debe rechazar la hipotesis 
# nula (H0). 

#-------------------------------------------------------------------------------
# PASO 5:
# interpretación:
# Como sí se ha rechazo la hipotesis nula, entonces se concluye que sí hay
# suficiente evidencia en la muestra para establacer que la proporción
# poblacional de estudiantes que cometieron fraude es menor que 56%, es decir,
# sí hubo una reducción significativa en la propoción poblacional de estudiantes
# que cometion fraude.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


# Margen de error (unilateral)
ME <- 1.64 * sqrt( pi.hat * (1 - pi.hat) / n )
# 0.08505873
# El margen de error (unilateral) de la estimacion es del 8.5%.

# Intervalo de confianza unilateral para pi
pi.hat + ME
# 0.4961698

# Interpretación: 
# El intervalo de confianza (unilateral) para la proporción de estudiantes que
# cometieron algun tipo de trampa es (0 , 49.6%), esto es, con una confiabilidad
# del 95%, se concluye que la proporcion poblacional de estuandtes que cometieron
# alguna clase de grande es infererior a 49.6%.