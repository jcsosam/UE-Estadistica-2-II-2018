#############
# Ejemplo 1 #
#############

##################################################################
#Stock Market Liquidity
#An investor's decision to purchase a stock is generally made with 
#a number of criteria in mind.
#Description:
#AVGT Average time between transactions, in minutes
#VOLUME Three months total trading volume, in millions of shares
#NTRAN Three months total number of transactions
#PRICE Opening stock price on January 2, 1985, in U.S. dollars
#SHARE Number of outstanding shares on December 31, 1984, in millions of shares
#VALUE Market equity value obtained by taking the product of PRICE and SHARE
#DEBEQ Debt-to-equity ratio, financial leverage
#TIC Company ticker symbol
#COMPANY Company name
##################################################################

#establecer el directorio de trabajo
setwd("C:/Users/Toshiba/Dropbox/UE/Estadistica 2  II-2018/data/")

#importar data
liquid <- read.table(file = "Liquidity.csv", header = TRUE, sep = ",")

#vizualizar la base de datos
View(liquid)

#adjuntar la base de datos
attach(liquid)

#matriz de diseño
X <- liquid[ , c("AVGT","NTRAN","PRICE","SHARE","VALUE","DEBEQ")]

#matriz de correlacion variables independientes
round(cor(X), 2)

#prisma de correlaciones     
windows()
pairs(VOLUME ~ AVGT + NTRAN + PRICE + SHARE + VALUE + DEBEQ)

#incluir NTRAN + SHARE + I(NTRAN^2) + I(SHARE^2) + NTRAN:SHARE

#ajuste modelo 1
M1 <- lm(VOLUME ~ NTRAN + SHARE + I(NTRAN^2) + I(SHARE^2) + NTRAN:SHARE, data = liquid)

summary(M1)

#el intercepto, NTRAN^2, NTRAN:SHARE resultan NO significativos
#Adjusted R-squared:      0.8525 
#Residual standard error: 4.083


#seleccion de variables usando Stepwise Regression
#install.packages("MASS")
library(MASS)
step <- stepAIC(M1, direction = "both")

step$anova

#Initial Model:
#VOLUME ~ NTRAN + SHARE + I(NTRAN^2) + I(SHARE^2) + NTRAN:SHARE
#                             
#Final Model:
#VOLUME ~ NTRAN + SHARE + I(SHARE^2)

#ajuste modelo 2 (modelo final con intercepto)
M2 <- lm(VOLUME ~ NTRAN + SHARE + I(SHARE^2), data = liquid)

summary(M2)

#el intercepto resulta NO signiticativo
#Adjusted R-squared:      0.8513 
#Residual standard error: 4.099

#ajuste modelo 3 (M2 sin el intercepto)
M3 <- lm(VOLUME ~ -1 + NTRAN + SHARE + I(SHARE^2), data = liquid)

summary(M3)

#Adjusted R-squared:      0.9427 
#Residual standard error: 4.091

#diagnosticos
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(M3)

#las observaciones 37, 79, 85 son (posiblemente) ouliers
#la observacion 122 es (posiblemente) influyente
#los outliers parecen deviar el supuesto de normalidad

#asjuste modelo 4 (sin outliers de M3)
M4 <- lm(VOLUME ~ -1 + NTRAN + SHARE + I(SHARE^2), data = liquid[-c(37, 79, 85), ])

summary(M4)

#Adjusted R-squared:      0.9554 
#Residual standard error: 3.46

#diagnosticos
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(M4)

#prueba de normalidad
shapiro.test(rstandard(M4))

#parece haber desvios de la distribucion normal 
#(la colas de la distribucion son pesadas)
#opciones:
#transformacion
#regresion robusta
#regresion rigida
#modelo lineal generalizado
#modelo no lineal

#ajuste modelo 5 (transformacion)
M5 <- lm(sqrt(VOLUME) ~ NTRAN + SHARE + I(NTRAN^2) + I(SHARE^2), data = liquid[-c(60, 79, 122), ])

summary(M5)

#Adjusted R-squared:      0.8479
#Residual standard error: 0.4914 

#dignosticos
windows()
layout(matrix(c(1,2,3,4), 2, 2)) # optional 4 graphs/page 
plot(M5)

#prueba de normales
shapiro.test(rstandard(M5))