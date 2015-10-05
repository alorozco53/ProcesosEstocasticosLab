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

## Construye la simulación de una cadena de Márkov {Xn}n>=0 en n pasos,
## dada una matriz de probabilidades de transición y un vector
## inicial. Regresa un vector con el valor de Xn.
## matrix: matriz de probabilidades de transición
## initial.distrib: distribución inicial
## n: tiempo; número de pasos de la simulación
MarkovSimulation <- function(trans.matrix, initial.distrib, n) {
    path <- c(RandomValue(initial.distrib))
    if (n != 0) {
        for (i in 2:(n+1))
            path[i] <- RandomValue(trans.matrix[path[i-1],])
    }
    return(path)
}

## Calcula NjM, el número de visitas al estado j en M pasos
## j: estado a simular, 0 <= j
## M: número de pasos, 1 <= M <= length(markov.chain)
NjM <- function(markov.chain, j, M) {
    njm <- 0
    for (i in 1:M) {
        if (markov.chain[i] == j+1)
            njm <- njm + 1
    }
    return(njm)
}

## Calcula la proporción del número de visitas a un estado j
## en M pasos, dada una cadena de Márkov,
## 1 <= M <= length(markov.chain)
AvgVisits <- function(markov.chain, j, M) {
    return(NjM(markov.chain, j, M) / M)
}

## Dados dos vectores x e y del mismo tamaño, regresa un nuevo
## vector cuya i-ésima entrada es abs(x[i]-y[i]). Esto con el fin
## de comparar los valores entrada por entrada.
VectorComparison <- function(x, y) {
    z <- c(0)
    for (i in 1:length(x))
        z[i] <- abs(x[i] - y[i])
    return(z)
}

## Devuelve el número de combinaciones de n elementos, de m en m
comb <- function(n, m) {
    return(factorial(n) / (factorial(m) * factorial(n-m)))
}
