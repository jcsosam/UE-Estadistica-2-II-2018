#################
### BOOTSTRAP ###
#################

# (Anderson 2011, Chap. 8., problem 47) Many stock market observers say that 
# when the PE ratio for stocks gets over 20 the market is overvalued. The PE 
# ratio is the stock price divided by the most recent 12 months of earnings.
# Suppose you are interested in seeing whether the current market is overvalued 
# and would also like to know what proportion of companies pay dividends. 
# A random sample of 30 companies listed on the New York Stock Exchange (NYSE) 
# is provided (Barron's, January 19, 2004).

# Objetivo: hacer inferencia sobre la proporcion poblacional de companias que
# efectivamente si producen dividendos.

# importar datos
NYSEStocks <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 2  II-2018/data/NYSEStocks.CSV")

# vizualizar datos
View(NYSEStocks)

# renombrar las variables
colnames(NYSEStocks) <- c("Compania","Dividendos","PE")

# hay datos faltantes?
any( is.na(NYSEStocks) )
# FALSE
# no hay companias con datos faltantes

n <- nrow(NYSEStocks)  # tamaño de la muestra
# Hay 30 companias en la muestra

# adjuntar la base de datos
attach(NYSEStocks)

# varible de estudio
x <- rep(NA, n)
x[Dividendos == "Yes"] <- 1
x[Dividendos == "No"] <- 0


#############################################
# Bootstrap sobre la proporcion poblacional #
#############################################

# numero de muestras (bootstrap)
M <- 100000

# almacenar los promedios
promedio.bootstrap <- matrix(NA, nrow = M, ncol = 1)

# semilla de la simulacion : la semilla se utiliza para que todos los que ejecuten
# la simulacion tengan los mismos resultados. El numero "1234" es arbitrario.
set.seed(seed = 1234)

# simulacion
for (i in 1:M) {
        muestra <- sample(x = x, size = n, replace = TRUE)
        promedio.bootstrap[i] <- mean( muestra )
        rm(muestra)
}

# grafico
windows(width = 10, height = 10)
hist(promedio.bootstrap, freq = F, density = 15, border = "gray", col = "orange",
     main = "Distribución Bootstrap", xlab = "Estimador", ylab = "Densidad")
abline(v = quantile(x = promedio.bootstrap, probs = c(0.025, 0.975)), col = "blue", lty = 2, lwd = 2)
abline(v = mean(promedio.bootstrap), col = "red", lty = 2, lwd = 2)

# estimacion Bootstrap
round(mean(promedio.bootstrap), 3)

# estimacion maxima verosimilitud
theta.hat <- mean(x)

# intervalo de confianza Bootstrap
round(quantile(x = promedio.bootstrap, probs = c(0.025, 0.975)), 3)

# intervalo de confianza maxima verosimilitud
round( theta.hat + c(-1, 1) * qnorm(p = 0.975) * sqrt(theta.hat * (1 - theta.hat) / n), 3)

# error estandar bootstrap
round( sd(promedio.bootstrap), 3 )

# error estandar maxima verosimilitud
round(sqrt(theta.hat * (1 - theta.hat) / n), 3)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
