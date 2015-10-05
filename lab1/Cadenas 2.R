f4 <- function(x, y, a) {
    if(x==a)
        return(a)
    else
        return(x+y)
}

CA.1babs <- function(x0, p, n, a) {
    y <- c(0)
    y[1] <- x0
    for(i in 2:n)
        y[i] <- f4(y[i-1],2*rbinom(1,1,p)-1,a)
    plot(y, type="o",main="CA1BAbs")
}

f5 <- function(x, y, a, b) {
    if(x==a) 
        return(a)
    else {
        if(x==b)
            return(b)
        else
            return(x+y)
    }
}

## Regresa una cadena de Márkov con dos barreras absorbentes
CA.2babs <- function(x0, p, n, a, b) {
    y <- c(0)
    y[1] <- x0
    for(i in 2:n)
        y[i] <- f5(y[i-1], 2*rbinom(1,1,p)-1, a, b)
    return(y)
}

## T_{a,-a}
## y: cadena de Márkov (vector)
## a: barrera absorbente
Ta <- function(y, a) {
    if(a==y[1] || y[1]==-a)
        return(0)
    for(i in 2:length(y)) {
        if(y[i]==a || y[i]==-a)
            return(i-1)
    }
    return(length(y)-1)
}

## Estima el tiempo esperado de llegada a alguna
## barrera absorbente a ó -a.
## Implementación idéntica a la que hizo el ayudante (no soy
## fan de esta implementación, sin embargo).
## n1: tamaño de las caminatas aleatorias
## n2: número de simulaciones
est.tabs <- function(p, n1, x0, a, n2) {
    h <- 0
    for (i in 1:n2) {
        y <- CA.2babs(x0, p, n1, a, -a)
        h <- h + Ta(y, a)
    }
    return(h/n2)
}
