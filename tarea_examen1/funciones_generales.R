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
NjM <- function(markov.chain, j, M) {
    njm <- 0
    for (i in 1:length(markov.chain)) {
        if (i == j+1)
            njm <- njm + 1
    }
    return(njm)
}

## Calcula la proporción del número de visitas a un estado j
## en M pasos, dada una cadena de Márkov
Avg.Visits <- function(markov.chain, j, M) {
    return(NjM(markov.chain, j, M) / M)
}

## Calcula la probabilidad de transición en m pasos (P^m)i,j
## vía las ecuaciones de Chapman-Kolmogorov, empezando en n.
## trans.function: recibe una función de probab. de trans.
## params: lista de parámetros extra de la función
## MStep <- function(trans.function, params, n, m, i, j) {
##     if (n+m == 0) {
##         if (i == j)
##             return(1)
##         else
##             return(0)
##     }
##     if (n+m == 1) {
##         params$i = i
##         params$j = j
##         return(trans.function(params))
##     }
##     probab <- 0
##     for (k in 0:max(i, j)) {
##         n.step <- MStep(trans.function, params, n-1, 1, i, k)
##         m.step <- MStep(trans.function, params, m-1, 1, k, j)
##         probab <- probab + (n.step * m.step)
##     }
##     return(probab)
## }
MStep <- function(trans.function, params, m, i, j) {
    if (m == 0) {
        if (i == j)
            return(1)
        else
            return(0)
    }
    if (m == 1) {
        params$i = i
        params$j = j
        return(trans.function(params))
    }
    probab <- 0
    for (k in 0:max(i, j)) {
        m.step <- MStep(trans.function, params, m-1, i, k)
        params$i = i
        params$j = j
        probab <- probab + (m.step * trans.function(params))
    }
    return(probab)
}
