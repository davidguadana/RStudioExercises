################################
# TEORÍA DE LA PROBABILIDAD
################################
##
## Estimación de Máxima Verosimilitud
##
################################
# Semana 6 - Clase 2/2

##### Ejercicio 1.1 ############

# Al inicio del Capítulo 10 se presentó la base de datos sobre medidas del cuerpo, consulte la explicación sobre la base de datos y responda lo siguiente. 
# Si se asume que la edad tiene distribución normal, ¿cuáles son los estimadores de máxima verosimilitud para μ y σ?

################################

# Base de datos:

url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=T)
head(datos)  # Para ver el encabezado de la base de datos


# vector "edad" que corresponde a una muestra aleatoria de una distribución normal con parámetro n = 36 conocido.

edad <- c(datos$edad)
edad

mean(x=edad)
sd(x=edad, na.rm = FALSE)

# Función de log-verosimilitud para la distribución normal (evaluada en la muestra "edad")

ll <- function(param) {
  media <- param[1]  # param es el vector de parámetros
  desvi <- param[2] 
  sum(dnorm(x=edad, mean=media, sd=desvi, log=TRUE))
}

ll1 <- function(a, b) sum(dnorm(x=edad, mean=a, sd=b, log=TRUE))
ll1 <- Vectorize(ll1)
xx <- seq(from=0, to=80, by=5)
yy <- seq(from=5, to=40, by=5)
zz <- outer(X=xx, Y=yy, ll1)
filled.contour(x=xx, y=yy, z=zz, nlevels=30,
               xlab=expression(mu), ylab=expression(sigma),
               color = topo.colors)

# Obteniendo los valores de μ y σ que maximizan la función de log-verosimilitud.

minusll <- function(x) -ll(x)
nlminb(objective=minusll, start=c(0, 5),
       lower=c(0, 5), upper=c(80, 40))

## $par
## [1] 31.44444 10.40685
## $objective
## [1] 135

# De la salida anterior podemos observar que los valores óptimos de μ y σ son 31.444 y 10.406 respectivamente. 
# Este resultado coincide con lo observado en la Figura y con los valores reales de simulación de la muestra. 
# Esto indica que el procedimiento de estimación de parámetros por máxima verosimilitud entrega valores insesgados de los parámetros a estimar.
# Un resultado interesante de la salida anterior es que se reporta el valor mínimo que alcanza la función minusll, este valor fue de 135, por lo tanto, se puede afirmar que el valor máximo de log-verosimilitud es -135.

# También podemos utilizar la función fitdistr del paquete básico MASS para obtener directamente el valor que maximiza la función log-verosimilitud según una distribución escogida.

fit1 <- fitdistr(edad, "normal")
fit1

## > fit1
## mean     sd  
## 31.44   10.41 
## ( 1.73) ( 1.23)

logLik(fit1)

## 'log Lik.' -135 (df=2)

# De esta última salida se observa que el valor coincide con el obtenido cuando se usó nlminb.

################################

##### Ejercicio 1.2 ############

# Como el histograma para la edad muestra un sesgo a la derecha se podría pensar que la distribución gamma sería una buena candidata para explicar las edades observadas.
# Asumiendo una distribución gamma, ¿cuáles son los estimadores de máxima verosimilitud para los parámetros?

################################

# Base de datos:

url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=T)
head(datos)  # Para ver el encabezado de la base de datos


# vector "edad" que corresponde a una muestra aleatoria de una distribución normal con parámetro n = 36 conocido.

edad <- c(datos$edad)
edad

mean(x=edad)
sd(x=edad, na.rm = FALSE)

# Asumiendo que la muestra aleatoria proviene de una gamma estimamos los parámetros de la distribución gamma.
# Utilizaremos la función fitdistr del paquete básico MASS para obtener directamente el valor que maximiza la función log-verosimilitud según una distribución escogida.

fit2 <- fitdistr(edad, "gamma")
fit2

## > fit2
## shape     rate  
## 10.8199    0.3441 
## ( 2.5118) ( 0.0818)

# De la salida anterior podemos observar que los estimadores de máxima verosimilitud para los parámetros de μ y σ son 10.8199 y 0.3441 respectivamente. 

################################

##### Ejercicio 1.3 ############

# ¿Cuál de los dos modelos es más apropiado para explicar la variable de interés? Calcule el AIC para decidir.

################################

# Base de datos:

url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=T)
head(datos)  # Para ver el encabezado de la base de datos


# vector "edad" que corresponde a una muestra aleatoria de una distribución normal con parámetro n = 36 conocido.

edad <- c(datos$edad)
edad

mean(x=edad)
sd(x=edad, na.rm = FALSE)

# Dibujaremos dos qqplot, uno asumiendo distribución normal y el otro distribución gamma.
# Lo haremos para visualizar cuál distribución se ajusta mejor a los datos simulados.

n <- length(edad)
n

par(mfrow=c(1, 2))

qqplot(y=edad, pch=19,
       x=qnorm(ppoints(n), mean=31.44444, sd=10.40685),
       main='Normal Q-Q Plot',
       xlab='Theoretical Quantiles',
       ylab='Sample Quantiles')

qqplot(y=edad, pch=19,
       x=qgamma(ppoints(n), shape=10.8199, rate=0.3441),
       main='Gamma Q-Q Plot',
       xlab='Theoretical Quantiles',
       ylab='Sample Quantiles')

# Se observa que al asumir normalidad, los puntos del qqplot tienden a no estar alineados, mientras que al asumir distribución gamma, los puntos sí tienden a estar alineados. 
# De esta figura se puede concluir que la muestra "edad" puede provenir de una Gamma.

# Para comparar modelos de forma más precisa, utilizamos el Akaike information criterion (AIC) propuesto por Akaike (1974) que sirve para medir la calidad relativa de los modelos estadísticos.
# Siempre el modelo elegido es aquel modelo con el menor valor de AIC. Ahora calculamos el AIC para los modelos asumidos normal y gamma.

-2 * logLik(fit1) + 2 * 2  # AIC para modelo normal

## 'log Lik.' 275 (df=2)

-2 * logLik(fit2) + 2 * 2  # AIC para modelo gamma

## 'log Lik.' 266 (df=2)

# De los resultados anteriores se concluye que entre los dos modelos, el mejor es el gamma porque su AIC = 266 es el menor de todos los AIC.




