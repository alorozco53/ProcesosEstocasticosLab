source('funciones_generales.R')

StreakMarkovProbab <- function(i, j, p) {
    if (j == 0)
        return(1 - p)
    else {
        if (j == i+1)
            return(p)
        else
            return(0)
    }
}

H <- function(x, y) {
    if (y == 0)
        return(0)
    else
        return(x + 1)
}

StreakMarkovChain <- function(x0, p, n) {
    markov.chain <- c(x0)
    for (i in 2:n)
        markov.chain[i] <- H(markov.chain[i-1], rbinom(1, 1, p))
    return(markov.chain)
}

MStepStreakMarkovChain <- function(m, i, j, params) {
    if (m == 0) {
        if (i == j)
            return(1)
        else
            return(0)
    }
    if (m == 1)
        return(StreakMarkovProbab(i, j, params$p))
    else {
        probab <- 0
        for (k in 0:(i+m)) {
            m.step <- MStepStreakMarkovChain(m-1, i, k, params)
            one.step <- StreakMarkovProbab(k, j, params$p)
            probab <- probab + (m.step * one.step)
        }
        return(probab)
    }
}

example <- StreakMarkovChain(0, 151/326, 250)
plot(example, type="o", col="red", main="Cadena de Rachas",
     xlab="partidas", ylab="racha de victorias")
