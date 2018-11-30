################################################
########## TEOREMA DEL LIMITE CENTRAL ##########
################################################

#-------------------------------------------------------------------------------
# Muestra aleatoria: conjunto de variables aleatorias X1, X2, ... , Xn 
# INDEPENDIENTES IDENTICAMENTE DISTRIBUIDAS.
# - Independientes: los valores de la muestra no depeden los unos de los otros.
# - Identicamente Distribuidas: cada varaible proviene de la misma poblacion.
# Muestreo CON reemplazo = IID.
# Muestreo SIN reemplazo = muestro con reemplazo cuando n << N.
# EL MUESTREO CON REEMPLAZO Y EL MUESTRO SIN REEMPLAZO SON EQUIVALENTES CUANDO EL
# TAMAÑO DE LA POBLACION ES GRANDE EN COMPARACION AL DE LA MUESTRA.
#-------------------------------------------------------------------------------


# Simular una muestra proveniente de una POBLACION con distribucion CHI CUADRADO 
# con 3 grados de libertad
set.seed(1)
datos.muestra <- rchisq(n = 40, df = 3)

# grafico de la poblacion
windows()
# curva
curve(dchisq(x, df = 3), from = 0, to = 15, col = "red", lwd = 2, lty = 1, 
      ylim = c(0, 0.28), xlab = "x", ylab = "Funcion de densidad")
# histograma
hist(datos.muestra, freq = FALSE, add = TRUE, border = "gray", 
     col = adjustcolor("gray", alpha.f = 0.1))
# leyenda
legend("topright", legend = c("Poblacion","Datos"), col = c("red","gray"), 
       lty = c(1, 2), lwd = 2)


# Promediar
# EL PROMEDIO MUESTRAL ES UNA VARIABLE ALEATORIA cuyo valor depende de la 
# muestra que ha sido seleccionada.

# matriz con 1000 filas y 4 columnas
datos.promedio = matrix(data = NA, nrow = 10000, ncol = 4)
dim(datos.promedio)

# tamaños de muestra de la simulacion
ns = c(3, 10, 20, 40)
ns

# simulacion
# i hace referencia a las filas
# j hace referencia a las columnas
for(j in 1:4) {
     for(i in 1:10000) {
          datos.promedio[i, j] = mean( rchisq(n = ns[j], df = 3) )
     }
}

colnames(datos.promedio) = c("xbarra n = 3","xbarra n = 10","xbarra n = 20","xbarra n = 40")

#################
# OBSERVACIONES #
#################

# 1. El promedio muestral conserva el promedio de la poblacion.
#    SIN IMPORTAR EL TAMAÑO DE LA MUESTRA, EL PROMEDIO DEL PROMEDIO MUESTRAL ES 
#    EL PROMEDIO DE LA POBLACION. 
#    PROMEDIAR CONSERVA LA LOCALIZACION.

apply(X = datos.promedio, MARGIN = 2, mean)

# 2. La variablidad de promedio muestral es menor que la variabilidad de la 
#    poblacion.
#    A MEDIDA QUE AUMENTA EL TAMAÑO DE LA MUESTRA, LA VARIABILIDAD DEL PROMEDIO 
#    DISMINUYE
#    PROMEDIAR DISMINUYE LA VARIABILIDAD Y EL SESGO.
#    CUANTO MAYOR SEA LA MUESTRA, MENOR ES LA INCERTIDUMBRE DE MUESTREO.

apply(X = datos.promedio, MARGIN = 2, var)

# 3. A medida que aumenta el tamaño de la muestra, la distribucion probabilistica
#    del promedio muestral es cada vez mas y mas simetrica.
#    SIN IMPORTAR CUAL SEA LA DISTRIBUCION DE LA POBLACION (SESGADA O SIMETRICA),
#    LA DISTRIBUCION PROBABILISTICA (DISTRIBUCION MUESTRAL) DEL PROMEDIO MUESTRAL 
#    ES NORMAL A MEDIDA QUE AUMENTA EL TAMAÑO DE LA MUESTRA.

windows(width = 10, height = 10)
par(mfrow = c(2,2))
#n=3
hist(datos.promedio[,1], freq = FALSE, main = "n = 3", xlim = c(0,10), xlab = " Promedio", ylim = c(0,1))
curve(dchisq(x, df = 3), from = 0, to = 10, col = "gray", lwd = 2, add = TRUE)
lines(density(datos.promedio[,1]), col = "blue", lwd = 2)
abline(v = mean(datos.promedio[,1]), col = "red")
#n = 10
hist(datos.promedio[ ,2], freq = FALSE, main = "n = 10", xlim = c(0,10), xlab = " Promedio", ylim = c(0,1))
curve(dchisq(x, df = 3), from = 0, to = 10, col = "gray", lwd = 2, add = TRUE)
lines(density(datos.promedio[,2]), col = "blue", lwd = 2)
abline(v = mean(datos.promedio[,1]), col = "red")
#n = 20
hist(datos.promedio[ ,3], freq = FALSE, main = "n = 20", xlim = c(0,10), xlab = " Promedio", ylim = c(0,1))
curve(dchisq(x, df = 3), from = 0, to = 10, col = "gray", lwd = 2, add = TRUE)
lines(density(datos.promedio[,3]), col = "blue", lwd = 2)
abline(v = mean(datos.promedio[,3]), col = "red")
#n = 40
hist(datos.promedio[ ,4], freq = FALSE, main = "n = 40", xlim = c(0,10), xlab = " Promedio", ylim = c(0,1))
curve(dchisq(x, df = 3), from = 0, to = 10, col = "gray", lwd = 2, add = TRUE)
lines(density(datos.promedio[,4]), col = "blue", lwd = 2)
abline(v = mean(datos.promedio[,4]), col = "red")

# En un caso practico: 
# 1. LA POBLACION ES DESCONOCIDA.
# 2. NO SE TOMAN 10000 MUESTRAS, SOLO SE ONSERVA UNA MUESTRA. 
# 3. SOLO SE TRABAJA UN TAMAÑO DE MUESTRA ESPECIFICO

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------