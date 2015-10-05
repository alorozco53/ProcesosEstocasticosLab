source('funciones_generales.R')

print('Ejercicio 6')

## Simula una cadena de Márkov de rachas.
## p: probabilidad de ganar una 'partida'
## x0: estado inicial (1 o 0, ganar o perder)
## n: número de 'partidas' a simular
StreakMarkovChain <- function(x0, p, n) {
    ## Función auxiliar que calcula el comportamiento
    ## de la cadena en cada paso.
    H <- function(x, y) {
        if (y == 0)
            return(0)
        else
            return(x + 1)
    }
    markov.chain <- c(x0)
    if (n > 1) {
        for (i in 2:n)
            markov.chain[i] <- H(markov.chain[i-1], rbinom(1, 1, p))
    } else
        markov.chain[2] <- H(markov.chain[1], rbinom(1, 1, p))
    return(markov.chain)
}

## Calcula la distribución estacionaria de la cadena de rachas
## con la fórmula que se derivó: π[0] = 1-p y para j >= 1,
## π[j] = π[j-1] * p. Regresa un vector con n valores de π.
## p: probabilidad de ganar una 'partida'
## n: tamaño del vector a devolver
StreakStationaryDistrib <- function(p, n) {
    distrib <- c(1 - p)
    if (n == 1)
        distrib[2] <- p * (1 - p)
    else {
        for (i in 2:(n+1))
            distrib[i] <- p * distrib[i-1]
    }
    return(distrib)
}

ex.x0 <- rbinom(1, 1, 1/2)
ex.p <- runif(1)
ex.n <- floor(1000 * (runif(1) + 0.01)) + 200
example <- StreakMarkovChain(ex.x0, ex.p, ex.n)
print('Parámetros:')
print(paste('x0:', ex.x0, ' '))
print(paste('p:', ex.p, ' '))
print(paste('n:', ex.n, ' '))
plot(example, type="o", col="red",
     main='Simulación de cadena de rachas',
     xlab="partidas", ylab="racha de victorias")

ex.stat.distrib <- StreakStationaryDistrib(ex.p, ex.n)
approx.pi <- c(0)
for (j in 0:(ex.n-1))
    approx.pi[j+1] <- AvgVisits(example, j, ex.n)
x11()
old.par <- par(mfrow=c(2,1))
plot(ex.stat.distrib, type="p", col="green",
     main='Distribución estacionaria de rachas',
     xlab="subconjunto de estados", ylab="valores para la distribución")
plot(approx.pi, type="p", col="blue",
     main='Aproximación de la distribución estacionaria de rachas',
     xlab="subconjunto de estados", ylab="valores para la distribución")
par(old.par)

x11()
plot(VectorComparison(approx.pi, ex.stat.distrib), type="p", col="black",
     main='Comparación entre distribuciones estacionarias',
     xlab="subconjunto de estados", ylab="diferencia entre ambas distribuciones")
