###############################
# TEORÍA DE LA PROBABILIDAD
###############################
##
## Estimación de Máxima Verosimilitud
##
##############################
# Semana 5 - Clase 2/2

##### Ejercicio 1.1 ############

# Calcular el valor de log-verosimilitud l(θ), asumiendo que p = 0.30 en la distribución binomial.

##############################

# vector rta que corresponde a una muestra aleatoria de una distribución binomial con parámetro n = 5 conocido.

rta <- c(2, 2, 1, 1, 1, 1, 0, 2, 1, 2,
         1, 0, 1, 2, 1, 0, 0, 2, 2, 1)

# Función de log-verosimilitud (evaluada en la muestra rta, el parámetro size como es conocido se reemplaza por el valor de 5, y en el parámetro prob se cambia por 0.3)

sum(dbinom(x=rta, size=5, prob=0.3, log=TRUE))

# Rpta: l(θ) = [1] -24.55231

##### Ejercicio 1.2 ############

# Construir una función llamada ll a la cual le ingrese valores del parámetro p de la binomial y que la función entregue el valor de log-verosimilitud.

##############################

ll <- function(prob) sum(dbinom(x=rta, size=5, prob=prob, log=T))

# Probando la función en dos valores arbitrarios p =  0.15 y p =  0.80 que pertenezcan al dominio del parámetro p de la distribución binomial:

ll(prob=0.15)  # Individual para p=0.15

ll(prob=0.80)  # Individual para p=0.80

# Vectorizando:

ll <- Vectorize(ll)

ll(prob=c(0.15, 0.80))

##### Ejercicio 1.3 ############

# Dibujar la curva log-verosimilitud l(θ), en el eje X debe estar el parámetro p del cual depende la función de log-verosimilitud.

##############################

curve(ll, lwd=4, col='dodgerblue3',
      xlab='Probabilidad de éxito (p)', las=1,
      ylab=expression(paste("Probabilidad de éxito (p=", theta, ")"))
)
grid()

##### Ejercicio 1.4 ############

# Observando la Figura generada, ¿cuál es el valor de p que maximiza la función de log-verosimilitud?

##############################

# Al observar la Figura se nota que el valor de p que maximiza la función log-verosimilitud está muy cerca de 0.2.

##### Ejercicio 1.5 ############

# ¿Cuál es el valor exacto de p que maximiza la función log-verosimilitud?

##############################

minusll <- function(x) -ll(x)

optimize(f=minusll, interval=c(0, 1))


##### Ejercicio 2.1 ############

# Construya la función de log-verosimilitud para los parámetros de la normal dada la muestra aleatoria y.

##############################

set.seed(1235)  # La semilla es 1235
y <- rnorm(n=50, mean=170, sd=5)
y[1:7]  # Para ver los primeros siete valores generados

ll <- function(param) {
  media <- param[1]  # param es el vector de parámetros
  desvi <- param[2] 
  sum(dnorm(x=y, mean=media, sd=desvi, log=TRUE))
}

ll1 <- function(a, b) sum(dnorm(x=y, mean=a, sd=b, log=TRUE))
ll1 <- Vectorize(ll1)
xx <- seq(from=160, to=180, by=0.5)
yy <- seq(from=3, to=7, by=0.5)
zz <- outer(X=xx, Y=yy, ll1)
filled.contour(x=xx, y=yy, z=zz, nlevels=20,
               xlab=expression(mu), ylab=expression(sigma),
               color = topo.colors)

minusll <- function(x) -ll(x)
nlminb(objective=minusll, start=c(163, 3.4),
       lower=c(160, 3), upper=c(180, 7))


require(MASS) # El paquete ya está instalado, solo se debe cargar
res <- fitdistr(x=y, densfun='normal')
res

logLik(res)


##### Ejemplo 3.1 ############

# Suponga que se desea estudiar una variable que tiene distribución Poisson con parámetro λ desconocido. Suponga además que se tienen dos situciones:
# Un solo valor 5 para estimar  
λ
.
Cuatro valores 5, 10, 6 y 15 para estimar  
λ
.
Dibujar la función  
l
(
  λ
)
para ambos casos e identificar la curvatura..

##############################








