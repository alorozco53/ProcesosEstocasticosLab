SimpleRandomWalk <- function(x0, p, n) {
    H <- function(x, y) {
        x + y
    }
    r.walk <- c(x0)
    if (n == 1)
        r.walk[2] <- H(r.walk[1], 2*rbinom(1,1,p)-1)
    else {
        for (i in 2:n)
            r.walk[i] <- H(r.walk[i-1], 2*rbinom(1,1,p)-1)
    }
    return(r.walk)
}

T <- function(r.walk) {
    if (length(r.walk) >= 3) {
        for (i in 3:length(r.walk))
            if (r.walk[i] > r.walk[i-1] &&
                r.walk[i-1] > r.walk[i-2])
                return(i)
    }
    return(length(r.walk))
}

est.expect.T <- function(p, x0, n1, n2) {
    sum.T <- 0
    for (i in 1:n2)
        sum.T <- T(SimpleRandomWalk(x0, p, n1))
    return(sum.T / n2)
}

symm.n <- floor(1000 * (runif(1) + 0.01)) + 200
symmetric.r.walk <- SimpleRandomWalk(0, 1/2, symm.n)
plot(symmetric.r.walk, type='o', col="brown",
     main="Caminata Aleatoria Simple Simétrica",
     xlab="tiempo", ylab="")

estim.n <- floor(1000 * (runif(1) + 0.01)) + 200
print('Estimación de la esperanza de T')
print(paste('p:', '1/2', ' '))
print(paste('x0:', '0', ' '))
print(paste('longitud de las simulaciones:', toString(symm.n), ' '))
print(paste('cantidad de iteraciones:', toString(estim.n), ' '))
print(paste('E[T]:', toString(est.expect.T(1/2, 0, symm.n, estim.n)), ' '))
