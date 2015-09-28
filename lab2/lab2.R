## Simula una distribución dada por un vector de
## probabilidades y devuelve el valor de la
## variable aleatoria que simula la distribución
## distrib: vector estocástico
RandomValue <- function(distrib) {
    x <- runif(1)
    cum.distrib <- cumsum(distrib)
    lower.bound <- 0
    for (i in 1:length(cum.distrib)) {
        if(x >= lower.bound && x <= cum.distrib[i])
            return(i)
        lower.bound <- cum.distrib[i]
    }
    return(lower.bound)
}

## Construye una trayectoria de estados con probabilidad
## de transición positiva para cualesquiera dos de ellos.
## matrix: matriz de probabilidades de transición
## initial.distrib: distribución inicial
## n: número de pasos (aristas) de la trayectoria
MarkovPath <- function(trans.matrix, initial.distrib, n) {
    path <- c(RandomValue(initial.distrib))
    if (n == 1)
        path[2] <- RandomValue(path[1])
    else {
        if (n != 0) {
            for (i in 2:(n+1))
                path[i] <- RandomValue(trans.matrix[path[i-1],])
        }
    }
    return(path)
}
