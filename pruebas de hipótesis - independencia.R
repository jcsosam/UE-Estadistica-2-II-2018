########################################################
### EJEMPLO 2: PRUEBAS CHI CUADRADO DE INDEPENDENCIA ###
########################################################

# importar datos

# vizualizar datos
View(INDEP)

#-------------------------------------------------------------------------------
# enfoque: 
#          1 = uso de software (no calculos manuales)
#          2 = ensenanza tradicional (calculos manuales)
#          3 = enfoque matematico con demostraciones
# carrera:
#          1 = administracion
#          2 = economia/finanzas
#          3 = matematicas/ingenieria
#          4 = otro
#
# hay diferencias en el modo de enseñanza? las variables son independientes?
#-------------------------------------------------------------------------------

# X = enfoque, f = 3
# Y = carrera, c = 4

# PASO 1
# Sistema de hipotesis:
# H0: el enfoque es independiente de la carrera
# frente a 
# H1: el enfoque NO es independiente de la carrera

alpha = 0.05

attach(INDEP)

# tabla de contingencia
tabla     = table(ENFOQUE, CARRERA)
tabla.abs = table(ENFOQUE, CARRERA)
tabla.rel = round(100 * tabla.abs / n, 1)

# 3 filas y 4 columnas con las FRECUENCIAS ABSOLUTAS

windows()
barplot(tabla.rel, beside = TRUE, col = c("red", "orange", "blue"), 
        names.arg = c("Admon","Ec/Fin","Mat/Ing","Otros"),
        xlab = "Carrera", ylab = "Porcentaje",
        main = "Enfoque por carrera")
legend("topright", c("Enfoque 1","Enfoque 2","Enfoque 3"), bty="n", 
       fill = c("red", "orange", "blue"))

n  = sum(tabla)   # tamaño de la muestra
#n = length(ENFOQUE)
nf = nrow(tabla)  # 3 numero de filas
nc = ncol(tabla)  # 4 numero de columnas

f = tabla                   # frecuencias observadas

perfil.f = rowSums(tabla)   # perfil fila: totales por fila
perfil.c = colSums(tabla)   # perfil columna: totales por columna

tabla.f = matrix(data = rep(perfil.f, nc), nrow = nf, ncol = nc, byrow = FALSE)
tabla.c = matrix(data = rep(perfil.c, nf), nrow = nf, ncol = nc, byrow = TRUE)

#cbind(perfil.f, perfil.f, perfil.f, perfil.f)
#rbind(perfil.c, perfil.c, perfil.c)

e = tabla.f * tabla.c / n  # frecuencias esperadas

# calculo del estadistico de prueba
est.prueba = sum( (f - e)^2 / e )
# el valor del estadistico de prueba calculado es chi^2 = 20.89 

# PASO 3
# establecer la region critica
percentil.chi = qchisq(p = 1 - alpha, df = (nf-1)*(nc-1), lower.tail = TRUE)
# 12.59159
# La region critica esta compuesta de todos los valores SUPERIORES a 12.59.
# La region critica es RC = { x : x > 12.59 }

# valor p
valor.p = pchisq(q = est.prueba, df = (nc - 1)*(nf - 1), lower.tail = FALSE)
# 0.001917688

# PASO 4:
# tomar la decision: 
# Como el estadistico de prueba chi^2 = 20.89 sí pertenece a la region critica (RC)
# ya que 20.89 > 12.59, entonces SI se rechaza la hipotesis nula (H0).

# PASO 5
# Como sí se ha rechazo la hipotesis nula, entonces se concluye que sí hay
# suficiente evidencia en la muestra para establacer que las variables del
# enfoque con el que se enseña y el pregrado donde se hace tal enseñanza NO
# son independientes.

#-------------------------------------------------------------------------------

# otra manera

chisq.test(x = tabla)

chisq.test(x = ENFOQUE, y = CARRERA)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
