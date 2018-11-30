##########################################################
### EJEMPLO 1: PRUEBAS CHI CUADRADO DE BONDAD DEAJUSTE ###
##########################################################

# importar datos
datos <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/ACOUNTS.txt", sep="")

# vizualizar datos
View(datos)

#-------------------------------------------------------------------------------
# DESCRIPTION
# numero de dias    categoria      proporcion (5 años pasados) 
# 0-14              1              0.72 = pi1^0
# 15-29             2              0.15 = pi2^0
# 30-59             3              0.10 = pi3^0
# 60 and more       4              0.03 = pi4^0
#
# OBJETIVO
# determinar si las proporciones han cambiado.
#-------------------------------------------------------------------------------

# la suma de todas las proporciones hipoteticas siempre es 100%
# k = 4 categorias

# PASO 1
# Sistema de hipotesis:
# H0: pi1 = 0.72, pi2 = 0.15, pi3 = 0.10, pi4 = 0.03
# frente a 
# H1: por lo menos alguna propocion es diferente al valor hipotetizado 
# Nivel de significancia es del 5%.

alpha = 0.05

# PASO 2
attach(datos)

ACOUNTS <- datos$ACOUNTS

n = length(ACOUNTS)            # 250 tamaño de la muestra

k = length( unique(ACOUNTS) )  # 4 categorias
#k = 4

p = c(0.72, 0.15, 0.10, 0.03)  # proporciones hipoteticas

f = table(ACOUNTS)             # frecuencias observadas
#   1   2   3   4 
# 159  28  47  16 

round(f / n, 2)                # frecuencias relativas
#    1    2    3    4 
# 0.64 0.11 0.19 0.06

e = n * p                      # frecuencias esperadas
# 180.0  37.5  25.0   7.5

conteos = cbind(f, e)
colnames(conteos) = c("Frecuencia", "Esperado")
#  Frecuencia Esperado
#1        159    180.0
#2         28     37.5
#3         47     25.0
#4         16      7.5

windows()
barplot(height = conteos, beside = TRUE, 
        col = c("red", "orange", "blue", "yellow"), 
        ylab = "Frecuencia", xlab = "Categoria")
legend("topright", c("Cat 1","Cat 2","Cat 3","Cat 4"), bty="n", 
       fill = c("red", "orange", "blue", "yellow"))

# calculo del estadistico de prueba
est.prueba = sum( (f - e)^2 / e )
# el valor calculado del estadistico de prubes es chi^2 = 33.85

# PASO 3
# establecer la region critica
percentil.chi = qchisq(p = 1 - alpha, df = k - 1, lower.tail = TRUE)
# 7.814728
# La region critica esta compuesta de todos los valores SUPERIORES a 7.81.
# La region critica es RC = { x : x > 7.81 }

# calculo valor p (PASO 3)
valor.p = pchisq(q = est.prueba, df = k - 1, lower.tail = FALSE)
# el valor p es igual a 2.131031e-07.

# PASO 4:
# tomar la decision: 
# Como el estadistico de prueba chi^2 = 33.85 sí pertenece a la region critica (RC)
# ya que 33.85 > 7.81, entonces SI se rechaza la hipotesis nula (H0).
# Equivalentemente, el valor p (aprox. 0%) indica que se debe favoreser la 
# hipotesis alternativa.

# PASO 5
# Como sí se ha rechazo la hipotesis nula, entonces se concluye que sí hay
# suficiente evidencia en la muestra para establacer que las proporciones 
# hipotetizadas de las categorias han cambiado significativamente para una 
# o mas categorias. Por lo tanto, la proporcion de clientes en las categorias
# de pago ha cambiado significativamente en comparacion de los últimos cinco 
# años.

#-------------------------------------------------------------------------------

# otra manera

chisq.test(x = f, p = p)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------