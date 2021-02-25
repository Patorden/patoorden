# disque agregamos unos cambios

Altura <- c(1.94, 1.82, 1.75, 1.80, 1.62, 1.64, 1.68, 1.46, 1.50, 1.55, 1.72, 1.67, 1.57, 1.60)
Peso <- c(98, 80, 72, 83, 65, 70, 67, 47, 45, 50, 70, 61, 50, 52)

cor(Altura, Peso) # correlacion de dos variables

m1 <- lm(Altura~Peso) # lm es linear model 
abline(lsfit(Altura, Peso)) # crea una gráfica con un modelo l
m1$residuals # te da los residuales
mean(m1$residuals) # te da la media de los residuales
sum(m1$residuals^2)/length(m1) # la varianza de los errores
summary(m1)
summary(Peso)
str(tabla)b

muestra = rnorm(n = 1000, mean=115, sd=7)
shapiro.test(muestra)
hist(muestra, breaks=50)
qqnorm(muestra)
qqline(muestra)

set.seed(123) # sin seed
A <- rnorm(50, 3, 0.5)

b <- c(26, 33, 65, 28, 34, 55, 25, 44, 50, 36, 26, 37, 43, 62, 35, 38, 45, 32, 28, 34)
bsort <- sort(b)
mean(b)
sd(b)

#########################################################
# EJERCICIO 1

# Primero hay que establecer el directorio de trabajo y 
# este deberá contener el archivo de datos production.txt

# Leemos nuestros datos con la función read.table

production <- read.table("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-05/Ejemplo-01/production.txt", 
                         header = TRUE)
# con read table, aunque la info no venga separada por commas,
# encuentra la manera y lo convierte a data frame 

download.file(production)

# Los datos que importamos a R se encuentran como data 
# frame con nombre production. Aplicamos la función attach 
# al data frame production para poder manipular las 
# columnas mediante sus nombres.

attach(production) 
# para ingresar a las variables sin el tabla$variable, solo variable

# Hacemos el gráfico de dispersión

plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)
summary(production)
str(production)
# Ajustamos un modelo de regresión lineal simple con la función 
# lm, en donde la variable de respuesta es RunTime y la variable 
# predictora es RunSize. Guardamos nuestro modelo ajustado con el nombre de m1.

m1 <- lm(RunTime~RunSize) # linear model runtime que depende de runsize 
# RunTime = m RunSize + intercept o y = mx + n 
# lm(y )
# te da el intercepto y la pendiente 

#Obtenemos un resumen de nuestro modelo ajustado mediante la función summary

summary(m1) # siempre hacer el summary 

# Graficamos nuestros datos nuevamente, ahora con la recta de 
# regresión estimada, mostrando algunas ecuaciones y algunos 
 # residuales gráficamente.

plot(RunSize, RunTime, xlab = "Tamaño de ejecución", 
     ylab = "Tiempo de ejecución", pch = 16)
abline(lsfit(RunSize, RunTime)) # Trazamos la recta de regresión estimada
# lsfit siginifica least squares 
mtext(expression(paste('Modelo de regresión lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=0, font = 3)

cor(RunTime,RunSize)
# Recta de regresión poblacional

text(x = 200, y = 240, expression(paste('Recta de regresión:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)


# Recta de regresión estimada

text(x = 350, y = 180, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada

text(x = 350, y = 160, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

# Residuales

points(189, 215, pch=16, col = "red") # Punto muestral con fnción points()
149.74770 + 0.25924 * 189 # Valor y sobre la recta estimada
lines(c(189, 189), c(198.7441, 215), col = "red") # linea del punto a la recta en y
# pones la coordenadas de y y x

points(173, 166, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 173 # Valor y sobre la recta estimada
lines(c(173, 173), c(166, 194.5962), col = "red")

# Acontinuación encontramos el cuantil de orden 0.975 de la distribución t 
# de Student con 18 (n - 2) grados de libertad. En total tenemos n = 20 
# observaciones en nuestro conjunto de datos. Estamos encontrando el valor 
# que satisface P(T > tval) = 0.025
# n -1 una cola n - 2 de dos colas
tval <- qt(1-0.05/2, 18) # asumiendo 18 grados de libertad 
tval

# Comprobamos

pt(tval, df = 18)

# Encontramos intervalos de confianza del 95% para el intercepto y la 
# pendiente del modelo de regresión lineal simple

round(confint(m1, level = 0.95), 3) 
# intervalo de confianza del 95% redondeado a 3 decimales 

# Ahora encontramos intervalos de confianza del 95% para la recta de regresión poblacional en algunos valores de X (RunSize)

RunSize0 <- c(50,100,150,200,250,300,350) # Algunos posibles valores de RunSize

(conf <- predict(m1, newdata = 
                   data.frame(RunSize = RunSize0), 
                 interval = "confidence", level = 0.95))

# Podemos visualizar gráficamente estos intervalos de confianza

lines(RunSize0, conf[, 2], lty = 2, lwd = 2, col = "green") # límites inferiores
lines(RunSize0, conf[, 3], lty = 2, lwd = 2, col = "green") # límites superiores

# También podemos encontrar intervalos de predicción del 95% para el valor real 
# de la variable de respuesta Y (RunTime) en algunos valores de X (RunSize)

(pred <- predict(m1, newdata = 
                   data.frame(RunSize = RunSize0), 
                 interval = "prediction", level = 0.95))

# Podemos visualizar gráficamente estos intervalos de predicción

lines(RunSize0, pred[, 2], lty = 2, lwd = 2, col = "blue") # límites inferiores
lines(RunSize0, pred[, 3], lty = 2, lwd = 2, col = "blue") # límites superiores

# Note como los intervalos de confianza están contenidos dentro de los 
# intervalos de predicción correspondientes.

# También es posible llevar a cabo un análisis de varianza para decidir si 
# existe asociación lineal entre RunSize y RunTime

anova(m1)

# Gráficos de diagnóstico de R
# Cuando usamos un modelo de regresión, hacemos una serie de suposiciones. 
# Entonces debemos hacer diagnósticos de regresión para verificar las 
# supocisiones.

par(mfrow = c(2, 2))
plot(m1)
dev.off()

#####################################################################
#EJERCICIO 2

# Supongamos que queremos emprender un negocio o que se nos solicita un estudio
# en en cual se requiere predecir el precio de cena (platillo), para poder 
# estar dentro de los rangos de precios del mercado y que el restaurante sea 
# rentable.

# Entonces primero vamos a analizar los datos de encuestas de clientes de 168 
# restaurantes Italianos en el área deseada que están disponibles, los cuales 
# tienen las siguientes variables de estudio:
  #Y: Price (Precio): el precio (en USD) de la cena
  #X1: Food: Valuación del cliente de la comida (sacado de 30)
  #X2: Décor: Valuación del cliente de la decoración (sacado de 30)
  #X3: Service: Valuación del cliente del servicio (sacado de 30)
  #X4: East: variable dummy: 1 (0) si el restaurante está al este (oeste) de la
      #quinta avenida

#Primero debemos establecer nuestro directorio de trabajo y el archivo de datos 
# (nyc.csv) que importaremos a R deberá de estar en este directorio.

nyc <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-2021/main/Sesion-05/Ejemplo-02/nyc.csv", 
                header = TRUE) 

# Observamos algunas filas y la dimensión del data frame

head(nyc, 2); tail(nyc, 2); dim(nyc)
attach(nyc)

# Llevamos a cabo el ajuste de un modelo Y = beta0 + beta1Food + beta2Decor + 
# beta3Service + beta4East + e

m1 <- lm(Price ~ Food + Decor + Service + East)

summary(m1)

# Ajustamos nuevamente un modelo pero ahora sin considerar la variable Service
# ya que en el resultado anterior se observó que su coeficiente de regresión no 
# fue estadísticamente significativo Y = beta0 + beta1Food + beta2Decor + 
# beta4*East + e (Reducido)

m2 <- lm(Price ~ Food + Decor + East) #como service no fue significativo se va
summary(m2)

# otra manera más sencilla:

m2 <- update(m1, ~.-Service)
summary(m2)

# Análisis de covarianza 

# Para investigar si el efecto de los predictores depende de la variable dummy 
# East consideraremos el siguiente modelo el cual es una extensión a más de 
# una variable predictora del modelo de rectas de regresión no relacionadas 
# Y = beta0 + beta1Food + beta2Decor + beta3Service + beta4East + beta5FoodEast
# + beta6DecorEast + beta7ServiceEast + e (Completo)

mfull <- lm(Price ~ Food + Decor + Service + East + 
               Food:East + Decor:East + Service:East)
summary(mfull)

# Ahora compararemos el modelo completo guardado en mfull contra el modelo 
reducido guardado en m2. Es decir, llevaremos a cabo una prueba de hipótesis general de
