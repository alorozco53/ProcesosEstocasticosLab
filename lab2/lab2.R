## Construye una trayectoria de estados con probabilidad
## de transición positiva para cualesquiera dos de ellos.
## Puede regresar trayectorias tan simples como loops hasta n.
## La longitud de la trayectoria es A LO MÁS n (n+1 estados)
## matrix: matriz de probabilidades de transición
## i0: estado inicial (donde va a empezar la trayectoria)
## n: (máximo) número de elementos de la trayectoria
markov.path <- function(matrix, i0, n) {
    path <- c(i0)
    node <- i0
    for(i in 2:(n+1)) {
        for(j in 1:dim(matrix)[1]) {
           if(matrix[node,j] > 0) {
                path[i] <- j
                node <- j
                break;
            }
        }
    }
    return(path);
}
