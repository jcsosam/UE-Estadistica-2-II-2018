#########################################################
### EJEMPLO: PRUEBAS DE HIPOTESIS VARIANZA POLACIONAL ###
#########################################################

# importar datoss

# vizualizar YIELDS
View(YIELDS)

# DESCRIPCION :
# yield : rendimientos semestrales de bonos gubernamentales (%).
#
# OBJETIVO :
# Comprobar por medio de pruebas de hipotesis si la variabilidad del rendimiento
# semestral es diferente de 0.70.

# X = redimientos (%), variable de interes

# PASO 1:
# El sistema de hipotesis es H0: sigma^2 = 0.7 frente a H1: sigma^2 != 0.7
# El valor hipotetico es sigma0^2 = 0.7.
# Con un nivel de significancia de alpha = 0.05

sigsq0 = 0.7

alpha  = 0.05

# PASO 2: 
# Calculo del estadistico de prueba chi^2

attach(YIELDS)

n  = length(yield)  ## 12 paises.
s2 = var(yield)     ## 0.689 varianza muestral (estimacion puntual de sigma^2)

est.prueba = (n - 1) * s2 / sigsq0
# el estadistico de prueba calculado es chi^2 = 10.84095

# PASO 3: 
# estalecer la region critica

percentil.chi.inf = qchisq(p = 0.025, df = n - 1, lower.tail = TRUE)
#3.815748

percentil.chi.sup = qchisq(p = 0.975, df = n - 1, lower.tail = TRUE)
#21.92005

# La region critica esta compuesta de todos los valores inferiores a 3.81 y
# superiores a 21.92.
# La region critica es RC = { x : x < 3.81 o x > 21.92 }

# valor p
p1 = pchisq(q = est.prueba, df = n - 1, lower.tail = TRUE) # area antes del est.prueba
#0.5433175
p2 = pchisq(q = est.prueba, df = n - 1, lower.tail = FALSE) # area despues del est.prueba
#0.4566825
valor.p = 2 * min(p1, p2)
# El valor p es 91.3%.

# PASO 4:
# tomar la desicion: 
# Como el estadistico de prueba chi^2 = 10.84 no pertenece a la region critica (RC)
# ya que 3.81 < 10.84 < 21.92, entonces NO se rechaza la hipotesis nula (H0).
# Equivalentemente, el valor p (91%) indica que NO se debe rechazar la hipotesis 
# nula (H0).

#-------------------------------------------------------------------------------
# PASO 5:
# interpretación:
# Como no se ha rechazo la hipotesis nula, entonces se concluye que no hay
# suficiente evidencia en la muestra para establacer que la variabilidad de 
# los rendimientos semestrales en 2009 es diferente a 0.7 que corresponde a la 
# variabilidad de los rendimientos en 2008.
#-------------------------------------------------------------------------------

# opcional: source prueba_varianza_una_poblacion_normal.R

prueba.varianza(x = yield, var0 = 0.7, alpha = 0.05, alternative = "diferente", decimales = 2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# usando intervalos de confianza
c( (n - 1) * s2 / percentil.chi.sup, (n - 1) * s2 / percentil.chi.inf )
# 0.3461975 1.9887755
# Con una confiabilidad del 95%, se concluye que la variabilidad de los rendimientos
# semestrales en 2009 se encuentre 0.35 y 1.99. Entonces se concluye que no hay
# suficiente evidencia en la muestra para establacer que la variabilidad de 
# los rendimientos semestrales en 2009 es diferente a 0.7 que corresponde a la 
# variabilidad de los rendimientos en 2008.