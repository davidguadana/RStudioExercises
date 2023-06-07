# EJERCICIOS DE TEORÍA DE LA PROBABILIDAD EN R

# EJERCICIO 1: Si \( Z \) es una variable con distribución normal estándar, calcula \( \mathbb{P}(-2.34 < Z < 4.78) \).
# SOLUCIÓN:

# Cálculo de la probabilidad acumulada en el límite inferior
pnorm(-2.34)
lower_prob <- pnorm(-2.34)

# Cálculo de la probabilidad acumulada en el límite superior
pnorm(4.78)
upper_prob <- pnorm(4.78)

# Calcula la probabilidad P(-2.34 < Z < 4.78)
prob <- upper_prob - lower_prob

# Imprime el resultado
print(prob)

# EJERCICIO 2: Calcula el rango intercuartílico de una población normal estándar.
# SOLUCIÓN:
# Rango Intercuartilico: diferencia entre el 3er y 1er cuartil

# Calcula el valor del primer cuartil (Q1)
qnorm(0.25)
q1 <- qnorm(0.25)

# Calcula el valor del tercer cuartil (Q3)
qnorm(0.75)
q3 <- qnorm(0.75)

# Calcula el rango intercuartílico
interquartile_range <- q3 - q1

# Imprime el resultado
print(interquartile_range)

# EJERCICIO 3: Genera una muestra de tamaño 10 de una población normal estándar. ¿Cuál es la diferencia entre la media muestral y la poblacional? Repite el ejercicio 3 veces y anota las 3 diferencias. 
# SOLUCIÓN:

# Especifica el tamaño de la muestra 1
n <- 10

# CASO 1:

# Genera la muestra de la población normal estándar
rnorm(n)
sample_1 <- rnorm(n)

# Imprime la muestra
print(sample_1)

# Calcula la media muestral
mean(sample_1)
sample_1_mean <- mean(sample_1)

# Calcula la diferencia entre la media muestral y la media poblacional
difference_1 <- sample_1_mean - 0

# Imprime el resultado
print(difference_1)

# CASO 2:

# Genera la muestra de la población normal estándar
rnorm(n)
sample_2 <- rnorm(n)

# Imprime la muestra
print(sample_2)

# Calcula la media muestral
mean(sample_2)
sample_2_mean <- mean(sample_2)

# Calcula la diferencia entre la media muestral y la media poblacional
difference_2 <- sample_2_mean - 0

# Imprime el resultado
print(difference_2)

# CASO 3:

# Genera la muestra de la población normal estándar
rnorm(n)
sample_3 <- rnorm(n)

# Imprime la muestra
print(sample_3)

# Calcula la media muestral
mean(sample_3)
sample_3_mean <- mean(sample_3)

# Calcula la diferencia entre la media muestral y la media poblacional
difference_3 <- sample_3_mean - 0

# Imprime el resultado
print(difference_3)

# EJERCICIO 4: Genera 1000 números con distribución de Poisson de parámetro \( \lambda = 1 \). Representa el gráfico de barras de los números obtenidos. Calcula la media y la varianza de los números obtenidos. ¿Se parecen a los valores teóricos? 
# SOLUCIÓN:

# Genera 1000 números con una distribución de Poisson de parámetro lambda = 1
lambda <- 1
n <- 1000
rpois(n, lambda)
numbers <- rpois(n, lambda)

# Representa el gráfico de barras de los números obtenidos
barplot(table(numbers), main = "Distribución de Poisson", xlab = "Número", ylab = "Frecuencia")

# Calcula la media de los números obtenidos
mean(numbers)
mean_value <- mean(numbers)

# Calcula la varianza de los números obtenidos
var(numbers)
variance_value <- var(numbers)

# Imprime la media y la varianza
cat("Media:", mean_value, "\n")
cat("Varianza:", variance_value, "\n")

# EJERCICIO 5: Calcula con R los siguientes valores: \( t_{3,\alpha} \), \( \chi^2_{3,\alpha} \), para \( \alpha = 0.05 \) y \( \alpha = 0.01 \). Compara los valores obtenidos con los que aparecen en las correspondientes tablas.
# SOLUCIÓN:

# Calcula los valores t y chi-cuadrado para los niveles de significancia alpha = 0.05 y alpha = 0.01
alpha_1 <- 0.05
alpha_2 <- 0.01
df <- 3

# Calcula los valores t
t_alpha_1 <- qt(1 - alpha_1/2, df)
t_alpha_2 <- qt(1 - alpha_2/2, df)

# Calcula los valores chi-cuadrado
chi2_alpha_1 <- qchisq(1 - alpha_1, df)
chi2_alpha_2 <- qchisq(1 - alpha_2, df)

# Imprime los resultados
print(paste("Valor t para alpha =", alpha_1, ":", t_alpha_1))
print(paste("Valor t para alpha =", alpha_2, ":", t_alpha_2))
print(paste("Valor chi-cuadrado para alpha =", alpha_1, ":", chi2_alpha_1))
print(paste("Valor chi-cuadrado para alpha =", alpha_2, ":", chi2_alpha_2))



