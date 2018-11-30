####################################
# Caso: Microestablecimientos 2016 #
####################################

#-------------------------------------------------------------------------------
# Información de los establecimientos con 9 o menos personas ocupadas en 
# comercio al por mayor, comercio al por menor y venta de motocicletas y sus 
# accesorios, y los talleres de mantenimiento y reparación de vehículos 
# automotores; todos los establecimientos de servicios, sin incluir los 
# financieros, el transporte, la educación pública y los establecimientos del 
# orden gubernamental (administración pública); y toda la microindustria según 
# la CIIU Rev. 4 A.C. Si estas actividades se desarrollan dentro de los hogares 
# también son objeto de medición. No se incluyen los puestos móviles.
#
# Información sobre las variables de actividad económica, personal ocupado,
# producción (industria), ventas (comercio) o ingresos (servicios), 
# organización jurídica, tiempo de funcionamiento, entre otras.
#
# Ver el archivo "Microestablecimientos.pdf" para más detalles.
#
# Fuente: http://microdatos.dane.gov.co/index.php/catalog/560/get_microdata
#-------------------------------------------------------------------------------

#################
# Base de datos #
#################

#micro <- read.delim("C:/Users/Juan Camilo/Dropbox/UE/VACACIONALES 2018/Estadistica 2/data/Microestablecimientos_2016.csv")
#View(micro)

dim(micro)
# 33013 microestablecimientos
# 104 variables

#-------------------------------------------------------------------------------

########################################################################
# Inferencia sobre la proporcion de microestablecimientos de servicios #
########################################################################

# el interes recae sobre la proporcion (porcentaje) de indiviuos que tienen
# una caracteristica objeto de estudio

sector <- micro$SECTOR
# 1 = industria
# 2 = comercio
# 3 = servicios

# hay datos faltantes?
any( is.na(sector) )
# FALSE
# el sector de los microestablecimientos no presentas datos faltantes

# tamaño de la muestra
n <- length(sector)
# 33015 microestablecimientos

# definir la variable de estudio
x <- rep(NA, n)
x[sector == 3] <- 1
x[sector != 3] <- 0

# estimar y hacer inferencia sobre la proporcion poblacional de microestablecimientos
# cuyo sector corresponde a servicios

#-------------------------------------------------------------------------------

### estadistica descriptiva

table(x)

tab.frec <- round( 100 * table(x)/n, 2)
#     0     1 
# 70.72 29.28

theta.hat <- sum(x) / n  # estimacion putual
# 0.292824
# Se estima que la proporcion de microestablecimientos de servicios es 29.3%.

windows()
barplot(height = tab.frec, names.arg = c("Otro","Servicios"), ylab = "Porcentaje", 
        col = "mistyrose", border = "red", main = "Microestablecimientos de servicios")
text(x = c(0.7, 1.9), y = tab.frec -3, labels = tab.frec, cex = 1.25)

#-------------------------------------------------------------------------------

### inferencia

# estimacion puntual
theta.hat <- mean(x)
# 0.292824 
# Estimacion puntual de la proporcion poblacional de microestablecimientos 
# de servicios es 29.28%.

# percentil 97.5 de la distribucion normal
z.975 <- qnorm(p = 0.975, mean = 0, sd = 1, lower.tail = TRUE)
# 1.959964

# margen de error (ME)
ME <- z.975 * sqrt( theta.hat * (1 - theta.hat) / n )
# 0.00490877 
# El margen de error de la estimación de la proporción poblacional de 
# microestablecientos de servicios es del 0.5% aproximadamente.

# intervalo de confianza
round( 100 * c(theta.hat - ME, theta.hat + ME), 2)

#-------------------------------------------------------------------------------
# INTERPRETACION
# con una confiabilidad del 95%, el porcentaje de microestablecimientos de servicios 
# se encuentra entre 28.79% y 29.77%.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

###################################################################################
# Inferencia sobre la media de los ingresos de microestablecimientos de servicios #
###################################################################################

ingresos <- micro$P958
#ingresos <- micro[ , "P958"]

# hay datos faltantes?
any( is.na(ingresos) )
# FALSE
# no hay ingresos faltantes

# X = ingresos de los microestablecimientos de servicios (en pesos).
# variable cuantitativa continua de razon
# definir la variable de estudio
x <- ingresos[sector == 3]  # datos ingresos sector servicios

n <- length(x)
# 9667
# tamaño de la muestra

#-------------------------------------------------------------------------------

### estadistica descriptiva

summary(x)
#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 15000   1800000   3900000   6777070   8000000 300000000 

# coeficiente de variacion
100 * sd(x) / mean(x) 
# Dado que el CV es 171.534%, se tiene que la variabilidad de los ingresos de los
# microestablecimientos es muy grande respecto al ingreso promedio.

windows()
boxplot(x, horizontal = TRUE, col = "mistyrose", border = "red", cex = 0.5, 
        boxwex = 0.5, xlab = "Ingresos", ylab = " ", 
        main = "Distribución ingresos microempresas de servicios")

#-------------------------------------------------------------------------------

### inferencia

# estimacion puntual
xbar <- mean(x)  # 6777070 valor promedio muestral de los ingresos
s    <- sd(x)    # 11624979 valor de la desv estandar muestral de los ingresos

# Se estima que el ingreso promedio de los microestablecimientos de servicios es 
# de $6,777,070.

# percentil 97.5 de la distribucion Normal estandar
z.975 <- qnorm(p = 0.975, mean = 0, sd = 1, lower.tail = TRUE)
# 1.959964

# margen de error
ME <- z.975 * s / sqrt( n )
# 231736.5
# El margen de error de la estimacion es de $231,736.5 con una confiabilidad del 95%.

# intervalo de confianza
round( c( xbar - ME, xbar + ME), 2)

# INTERPRETACION
# Con una confiabilidad del 95%, se concluye que el ingreso medio de los 
# microestablecimientos de servicios se encuentra entre $6,545,334 y $7,008,806.

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------