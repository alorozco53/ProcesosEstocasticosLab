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
        for (i in 2:(n+1))
            path[i] <- RandomValue(trans.matrix[path[i-1],])
    }
    return(path)
}

## Calcula NjM, el número de visitas al estado j en M pasos
## j: estado a simular, 0 <= j <= M
## M: número de pasos
NjM <- function(trans.matrix, initial.distrib, j, M) {
    markov.chain <- MarkovSimulation(trans.matrix,
                                     initial.distrib,
                                     M)
    njm <- 0
    for (i in 1:length(markov.chain)) {
        if (i == (j+1))
            njm <- njm + 1
    }
    return(njm)
}

Avg.Visits <- function(trans.matrix, initial.distrib, j, M) {
    return(NjM(trans.matrix, initial.distrib, j, M) / M)
}
