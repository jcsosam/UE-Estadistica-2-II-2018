#####################################################################
### EJEMPLO: PRUEBAS DE HIPOTESIS DIFERENCIA DE MEDIAS POLACIONAL ###
#####################################################################

# importar datos
PUR <- read.delim("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/PUR.txt")

# vizualizar PUR
View(PUR)

# DESCRIPCION :
# DIRECT : rendimientos anuales netos adquiridos directamente.
# BROKER : rendimientos anuales netos adquiridos por medio de un intermediario.
#
# OBJETIVO :
# Comprobar por medio de pruebas de hipotesis si fondos mutuos comprados directamente 
# superan fondos mutuos comprados a través de corredores.

#############################################
# PRUEBA DE HIPOTESIS COCIENTE DE VARIANZAS #
#############################################

# X = rendimientos anuales (variable de interés)

#----------------------------------------#
# POBLACION 1 = inversion directa        #
# POBLACION 2 = inversion intermediarios #
#----------------------------------------#

# PASO 1:
# El sistema de hipotesis es:
# H0: sigma^2_1/sigma^2_2 = 1 frente a H1: sigma^2_1/sigma^2_2 != 1

# El valor hipotetico es 1.
# Con un nivel de significancia de alpha = 0.05

alpha = 0.05

# PASO 2: 
# Calculo del estadistico de prueba f

attach(PUR)

s2.1 = var(Direct) # 37.48818 varianza muestral (inversion directa)
s2.2 = var(Broker) # 43.33928 varianza muestral (inversion intermediario)
n1   = length(Direct)  # 50 tamaño de muestra (inversion directa)
n2   = length(Broker)  # 50 tamaño de muestra (ivnersion intermediario)

est.prueba = s2.1/s2.2
# el estadistco de prueba calculado es f = 0.8649931

# PASO 3: 
# estalecer la region critica
percentil.f1 = qf(p = alpha/2, df1 = n1 - 1, df2 = n2 - 1, lower.tail = TRUE)
#0.5674762

percentil.f2 = qf(p = 1-alpha/2, df1 = n1 - 1, df2 = n2 - 1, lower.tail = TRUE)
#1.762189

# La region critica esta compuesta de todos los valores inferiores a 0.57 o
# los valores superiores a 1.76.
# La region critica es RC = { x : x < 0.57 o x > 1.76 }

# Valor p
p1 = pf(q = est.prueba, df1 = n1 - 1, df2 = n2 - 1, lower.tail = TRUE)
#0.3068444

p2 = pf(q = est.prueba, df1 = n1 - 1, df2 = n2 - 1, lower.tail = FALSE)
#0.6931556

valor.p = 2 * min(p1, p2)
# El valor p = 0.6136888, lo cual indica que NO se debe rechazar la hipotesis 
# nula (H0).

# PASO 4:
# tomar la desicion: 
# Como el estadistico de prueba f = 0.84 no pertenece a la region critica (RC)
# ya que 0.57 < 0.84 < 1.76, entonces NO se rechaza la hipotesis nula (H0).
# Equivalentemente, el valor p (61%) implica la homogeneidad de la variabilidad 
# en las dos poblaciones.

# PASO 5:
# interpretación:
# Como no se ha rechazo la hipotesis nula, entonces se concluye que no hay
# suficiente evidencia en la muestra para establacer que hay diferencias 
# significativas entre la variabilidad de los rendimientos de inversion directa 
# y por intermediario. Esto quiere decir que variabilidad de los rendimientos en 
# las dos poblaciones son HOMOGENEAS.

#-------------------------------------------------------------------------------

# intervalo de confianza (bilateral)

c( percentil.f1 * s2.1 / s2.2, percentil.f2 * s2.1 / s2.2 )
# 0.490863 1.524281

############################################
# PRUEBA DE HIPOTESIS DIFERENCIA DE MEDIAS #
############################################

#----------------------------------------#
# POBLACION 1 = inversion directa        #
# POBLACION 2 = inversion intermediarios #
#----------------------------------------#

# PASO 1:
# El sistema de hipotesis es:
# H0: mu1 - mu2 <= 0 frente a H1: mu1 - mu2 > 0
# El valor hipotetico es delta0 = 0.
# Con un nivel de significancia de alpha = 0.05

alpha  = 0.05

delta0 = 0

# PASO 2: 
# Calculo del estadistico de prueba t

xb1  = mean(Direct)    # el promedio muestral de inversion directa es 6.6312
xb2  = mean(Broker)    # el promedio muestral de intermediarios es 3.7232
s2.1 = var(Direct)     # 37.48818 varianza muestral (inversion directa)
s2.2 = var(Broker)     # 43.33928 varianza muestral (inversion intermediario)
n1   = length(Direct)  # 50 tamaño de muestra (inversion directa)
n2   = length(Broker)  # 50 tamaño de muestra (ivnersion intermediario)

sp   = sqrt( ( (n1 - 1)*s2.1 + (n2 - 1)*s2.2 ) / (n1 + n2 - 2) )  # 6.357179

est.prueba = ( xb1 - xb2 - delta0 )/( sp * sqrt(1/n1 + 1/n2) )
# entonces el estadistico de prueba calculado es t  = 2.287178

# PASO 3: 
# estalecer la region critica
percentil.t = qt(p = 1-alpha, df = n1 + n2 - 2, lower.tail = TRUE)
#1.660551

# La region critica esta compuesta de todos los valores superiores a 1.66.
# La region critica es RC = { x : x > 1.66 }

# CALCULO del valor p
valor.p = pt(q = est.prueba, df = n1 + n2 - 2, lower.tail = FALSE)
# El valor p = 0.01216805, lo cual indica que SÍ se debe rechazar la hipotesis 
# nula (H0).

# PASO 4:
# tomar la desicion: 
# Como el estadistico de prueba t = 2.28 sí pertenece a la region critica (RC)
# ya que 2.28 > 1.66, entonces SÍ se rechaza la hipotesis nula (H0).

# PASO 5:
# interpretación:
# Como sí se ha rechazo la hipotesis nula, entonces se concluye que sí hay
# suficiente evidencia en la muestra para establacer que, en promedio, los
# rendimiento directos son superiores que al llevarse acabo con intermediarios.

#-------------------------------------------------------------------------------

# otra menera
t.test(x = Direct, y = Broker, alternative = "greater", mu = 0, paired = FALSE, 
       var.equal = TRUE, conf.level = 0.95)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------