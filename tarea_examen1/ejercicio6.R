source('funciones_generales.R')

StreakMarkovProbab <- function(params) {
    if (params$j == 0)
        return(1 - params$p)
    else {
        if (params$j == params$i+1)
            return(params$p)
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


example <- StreakMarkovChain(0, 151/326, 250)
plot(example, type="o", col="red", main="Cadena de Rachas",
     xlab="partidas", ylab="racha de victorias")
print(MStep(StreakMarkovProbab,
            list(p=1/3), 2, 6, 0))
