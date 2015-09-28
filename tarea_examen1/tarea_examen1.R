## Simula una distribución dada por un vector de
## probabilidades y devuelve el valor de la
## variable aleatoria que simula la distribución
## distrib: vector estocástico
RandomValue <- function(distrib) {
    x <- runif(1)
    cum.distrib <- cumsum(distrib)
    lower.bound <- 0
    for (i in 1:length(cum.distrib)) {
        if (x >= lower.bound && x <= cum.distrib[i])
            return(i)
        lower.bound <- cum.distrib[i]
    }
    return(lower.bound)
}

## Construye la simulación de una cadena de Márkov {Xn}n>=0 en n pasos
## Regresa un vector con el valor de Xn.
## matrix: matriz de probabilidades de transición
## initial.distrib: distribución inicial
## n: tiempo; número de pasos de la simulación
MarkovSimulation <- function(trans.matrix, initial.distrib, n) {
    path <- c(RandomValue(initial.distrib))
    if (n != 0) {
        for (i in 2:n)
            path[i] <- RandomValue(trans.matrix[path[i-1],])
    }
    return(path)
}

## Ejercicio 1
print('Ejercicio 1')
print('ejemplo con parámetro p = 1/2')
p <- 1/4

print('matriz de probabilidades de transición P:')
P <- matrix(c(0), 4, 4)
P[1,2] <- P[3,2] <- P[2,4] <- p
P[1,1] <- P[3,1] <- P[2,3] <- 1-p
P[4,2] <- 1
print(P)

## a)
print('a)')
print('distribución inicial de ejemplo:')
init <- (1/4) * c(1, 1, 1, 1)
print(init)
## Tiempo de simulación
print('tiempo de simulación de ejemplo:')
n <- 100
simulation <- MarkovSimulation(P, init, n)
print(simulation)
plot(simulation, type="o", col="green",
     main="Cadena de Márkov con probabilidad de transición P",
     xlab="tiempo", ylab="")
